import pandas as pd
import numpy as np
import xgboost as xgb
import shap
import matplotlib.pyplot as plt

def plot_sensitivity_evolution(
    data_path,
    technology_name,
    region_name,
    discount_rate
):
    """
    Plots the evolution of SHAP parameter sensitivity (including scenario toggles
    as features) for a given technology and region as carbon prices change.
    """
    # 1. Load the Monte Carlo results
    print(f"Loading data from {data_path}...")
    df = pd.read_csv(data_path)

    # Define the discrete scenario steps to track (carbon prices)
    c_prices = sorted(df['c_price'].unique())
    print(f"Detected carbon prices: {c_prices}")

    # Dictionary to store the mean SHAP importance for each feature at each C-price
    importance_tracker = {}

    # Define columns to drop (note: allow_eor, early_adoption, plant_mw_th are NOT dropped)
    cols_to_drop = [
        'scenario_id', 'mc_run_id', 'region', 'technology', 'c_price', 'discount_rate',
        'area_best_km2', 'area_viable_km2', 'biomass_processed_yr_mg',
        'npv_min', 'npv_max', 'npv_mean'
    ]
    cols_to_drop += [col for col in df.columns if any(col.startswith(p) for p in ['mean_', 'total_'])]

    # 2. Iterate through Carbon Prices and fit SHAP model
    for cp in c_prices:
        # Filter conditionally to the specific technology/region/discount rate
        sub_df = df[
            (df['technology'] == technology_name) &
            (df['region'] == region_name) &
            (df['discount_rate'] == discount_rate) &
            (df['c_price'] == cp) &
            (df['npv_mean'].notna())
        ].copy()

        if len(sub_df) < 30:
            print(f"Skipping c_price={cp}: only {len(sub_df)} runs available (locked out or inactive)")
            continue

        print(f"Processing c_price={cp} ({len(sub_df)} runs)...")

        y = sub_df['npv_mean']
        X = sub_df.drop(columns=[c for c in cols_to_drop if c in sub_df.columns])
        
        # Convert boolean columns to integer to avoid issues with SHAP/XGBoost
        for col in X.columns:
            if X[col].dtype == bool or X[col].dtype == 'object':
                # Try to map True/False strings or booleans to 1/0
                if X[col].dtype == bool:
                    X[col] = X[col].astype(int)
                else:
                    X[col] = X[col].map({'TRUE': 1, 'FALSE': 0, 'True': 1, 'False': 0}).fillna(X[col])

        X = X.loc[:, X.var() > 1e-8] # Drop constants

        # Train XGBoost Regressor
        model = xgb.XGBRegressor(n_estimators=100, max_depth=5, learning_rate=0.05, random_state=42)
        model.fit(X, y)

        # Calculate SHAP values
        explainer = shap.TreeExplainer(model)
        shap_values = explainer(X)

        # Calculate Mean Absolute SHAP (Feature Importance)
        mean_abs_shap = np.abs(shap_values.values).mean(axis=0)

        # Store in tracker
        for idx, feat in enumerate(X.columns):
            if feat not in importance_tracker:
                importance_tracker[feat] = []
            importance_tracker[feat].append((cp, mean_abs_shap[idx]))

    if not importance_tracker:
        print("Error: No data populated in sensitivity tracker. Verify scenario filters.")
        return

    # 3. Plot the Evolution
    plt.figure(figsize=(12, 7))

    # Filter to top 6 most important features overall to avoid chart clutter
    overall_importance = {feat: np.mean([val[1] for val in data]) for feat, data in importance_tracker.items()}
    top_features = sorted(overall_importance, key=overall_importance.get, reverse=True)[:6]

    for feat in top_features:
        data = importance_tracker[feat]
        x_vals = [d[0] for d in data]
        y_vals = [d[1] for d in data]
        plt.plot(x_vals, y_vals, marker='o', linewidth=2.5, label=feat)

    plt.title(
        f"Evolution of Parameter Sensitivity vs. Carbon Price (Scenario Toggles as Features)\n"
        f"{technology_name} in {region_name} (DR={discount_rate*100}%)",
        fontsize=12, fontweight='bold', pad=15
    )
    plt.xlabel("Carbon Price ($/tCO2e)", fontsize=11)
    plt.ylabel("Mean Absolute SHAP Value (Impact on NPV)", fontsize=11)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', title="Key Parameters / Toggles")
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.tight_layout()

    # Save the figure
    output_png = f"results/sensitivity_evolution_{technology_name}_{region_name}_toggles.png"
    plt.savefig(output_png, dpi=300, bbox_inches='tight')
    print(f"Plot saved successfully to {output_png}")
    try:
        plt.show()
    except Exception as e:
        print(f"Could not display plot interactively (headless environment): {e}")

if __name__ == '__main__':
    data_path = 'results/mc_analysis_results.csv'
    technology_name = 'BECCS'
    region_name = 'Europe'
    discount_rate = 0.08
    
    plot_sensitivity_evolution(
        data_path=data_path,
        technology_name=technology_name,
        region_name=region_name,
        discount_rate=discount_rate
    )
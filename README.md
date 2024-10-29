# 📈 Analysis of the Wines Quality

The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. Contains some technical information in each observation, such as alcohol, acids or pH

## 📦 Dependencies & Packages

- GCC Version: +12.3.0
- GFORTRAN Version: +12.3.0 
- R language, and RStudio (optional)
- The necessary packages are in `packages.r`

## 🧪 Tests used to date (to do)

## 💾 Context of variables

Modify Odor:

- volatile acidity: High levels can generate a vinegar smell, negatively impacting the aroma.
- free sulfur dioxide: Acts as a preservative, but at high levels it can negatively affect the aroma, causing a sulfur smell.
- total sulfur dioxide: It can change the odor, especially if it is in excessive quantities, leaving an unpleasant chemical smell.
- alcohol: The alcohol content can influence the aroma, giving a sensation of heat and volatilizing other aromatic compounds.

Modify Flavor:

- fixed acidity: Affects the structure and overall acidity, directly impacting the perception of the wine's flavor.
- volatile acidity: Very high levels can leave a sour taste, like vinegar, becoming a flavor defect.
- citric acid: Adds freshness and a slight acidity, influencing the balance of flavors.
- residual sugar: Residual sugar levels alter the sweetness and body of the wine, affecting the taste.
- chlorides: They influence the flavor balance, with excess salt negatively impacting the final flavor.
- sulphates: Contribute to the sensation of astringency, influencing the texture and flavor of the wine.
- alcohol: The alcohol content affects the perception of body and heat in the wine, directly influencing the flavor.

Modify Color:

- pH: Higher acidity levels (low pH) can alter color intensity, especially in red wines.
- sulphates: In small quantities, they can help preserve the color of the wine by preventing oxidation.
- chlorides: Although its effect on color is not as direct as other components, imbalances in chloride levels can indirectly impact the brightness and appearance of the wine.

## 📃 References

1. UCI Machine Learning Repository: Wine Quality Data Set. (n.d.). Retrieved from https://archive.ics.uci.edu/dataset/186/wine+quality.

2. Yasser H. (n.d.). Wine Quality Dataset. Kaggle. Retrieved from https://www.kaggle.com/datasets/yasserh/wine-quality-dataset.
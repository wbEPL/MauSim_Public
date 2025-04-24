
# MauSim Tool Instructions

This folder contains the minimum set of files required to run the MauSim tool. To do this, please follow these steps:

1. Download the MauSim 2025 folder shared on dropbox or one drive to your computer.
   
2. Make a copy of the tool saved in `".../03-Outputs/template/MRT_Sim_tool_VI.xlsx"` into the folder where you will save your outputs. If those will be saved in the shared folder, please saved them into a subfolder with your username as follows:
   `".../03-Outputs/username/"`

3. Change the desired parameters in the tool and save the scenario with a _short_ name and add the label of the scenario in the worksheet named “General Parameters”.

4. Modify the parameters in blue as desired.

5. Save and close the Excel file.

6. Open the script `".../02-Scripts/00. Master.do"` and add the paths for: i) projects, ii) data, and iii) scripts within brackets with your username (see example below):

```stata
if "`c(username)'"=="wb419055" {
    * Running R or Stata in the folder
    local Software "Stata"
    
    * Project path. Ideally where results are going to be saved
    global path "C:\Users\wb419055\OneDrive - WBG\AWCF1 Poverty_Equity\04 MAURITANIA\PROJECTS\01 MRT Fiscal Incidence Analysis\0_Public_repository\v4.0_April_2025\"
    
    * Data Path. Ideally where data is being saved, Dropbox folder to ensure we are always using the latest data. However, if you have connection problems, you can download the data to your local computer
    global pathdata "C:\Users\wb419055\OneDrive - WBG\AWCF1 Poverty_Equity\04 MAURITANIA\PROJECTS\01 MRT Fiscal Incidence Analysis\0_Public_repository\v4.0_April_2025\01-Data"
    
    * DO files path: Ideally will be also saved in the project's path but Github gives the advantage of saving them wherever you want. Important: If you save them in the project's folder, you need to add a subfolder within scripts
    global thedo "${path}/02-Scripts/`c(username)'/`Software'"
}
```


# Instructions pour l'outil MauSim

Ce dossier contient l'ensemble minimal de fichiers nécessaires pour exécuter l'outil MauSim. Pour ce faire, veuillez suivre les étapes suivantes :

1. Téléchargez le dossier MauSim 2025 sur votre ordinateur.
   [Télécharger le dossier MauSim 2025](https://www.dropbox.com/scl/fo/ss1uivnptb8ch7hrhbik2/APN6KzeB6qfibGRwi-Lb0zg?rlkey=x0lbh1j2l4h95rx2h853yjg5k&st=14idx9ah&dl=0)

2. Faites une copie de l'outil enregistré dans `".../03-Outputs/template/MRT_Sim_tool_VI.xlsx"` dans le dossier où vous enregistrerez vos résultats. Si ceux-ci doivent être enregistrés dans le dossier partagé, veuillez les enregistrer dans un sous-dossier avec votre nom d'utilisateur comme suit :
   `"... ...\03-Outputs\username/"`

3. Modifiez les paramètres souhaités dans l'outil et enregistrez le scénario avec un _nom court_ et ajoutez l'étiquette du scénario dans la feuille de calcul nommée "Paramètres Généraux".

4. Modifiez les paramètres en bleu selon vos besoins.

5. Enregistrez et fermez le fichier Excel.

6. Ouvrez le script `".../02-Scripts/00. Master.do"` et ajoutez les chemins pour : i) les projets, ii) les données, et iii) les scripts entre crochets avec votre nom d'utilisateur (voir l'exemple ci-dessous) :

```stata
if "`c(username)'"=="wb419055" {
    * Exécution de R ou Stata dans le dossier
    local Software "Stata"
    
    * Chemin du projet. Idéalement là où les résultats seront enregistrés
    global path "C:\Users\wb419055\OneDrive - WBG\AWCF1 Poverty_Equity\04 MAURITANIA\PROJECTS\01 MRT Fiscal Incidence Analysis\0_Public_repository\v4.0_April_2025\"
    
    * Chemin des données. Idéalement là où les données sont enregistrées, dossier Dropbox pour s'assurer que nous utilisons toujours les dernières données. Cependant, si vous avez des problèmes de connexion, vous pouvez télécharger les données sur votre ordinateur local
    global pathdata "C:\Users\wb419055\OneDrive - WBG\AWCF1 Poverty_Equity\04 MAURITANIA\PROJECTS\01 MRT Fiscal Incidence Analysis\0_Public_repository\v4.0_April_2025\01-Data"
    
    * Chemin des fichiers DO : Idéalement, ils seront également enregistrés dans le chemin du projet, mais Github offre l'avantage de les enregistrer où vous voulez. Important : Si vous les enregistrez dans le dossier du projet, vous devez ajouter un sous-dossier dans les scripts
    global thedo "${path}/02-Scripts/`c(username)'/`Software'"
}

import csv
import json
from datetime import datetime
  
contenu = {} 

chemin = "concentrations-polluants-dans-lair-ambiant.json"

# Boucle pour charger le fichier JSON - redemande le chemin si fichier introuvable
while True :
    try:
        f = open(chemin)
        contenu = json.load(f)
        break # Sort de la boucle si le chargement réussit
    except FileNotFoundError:
        print(" ✘ VOTRE FICHIER : ", chemin, " EST INTROUVABLE !")
    except Exception as e:
        print(" ✘ UNE ERREUR EST SURVENUE.")
        print(e)
        
    chemin = input("Veuillez saisir le chemin d'accès du fichier Json.\n ➯")  

# Initialisation des variables pour le traitement
LongueurContenu = len(contenu)
LignesInvalides = 0
LignesFiltrees = []

# Boucle principale : parcourt chaque ligne du fichier JSON
for i in range (LongueurContenu):
    print("=" * 49)
    print("TRAITEMENT DE LA LIGNE ", str(i+1), "/", str(LongueurContenu))
    
    # Vérification et extraction du nom de la station
    if 'nom_station' not in contenu[i]['fields']:
        contenu[i]["fields"]['nom_station'] = "-" # Remplace par "-" si absent
    print("Ville : ", contenu[i]["fields"]['nom_station'])
    
    # Vérification des coordonnées GPS
    if 'coordinates' not in contenu[i]['geometry']:
        contenu[i]["geometry"]['coordinates'][0]= "-"
        contenu[i]["geometry"]['coordinates'][1]= "-"
    print("Coordonnées GPS : [", contenu[i]['geometry']["coordinates"][1], ",", contenu[i]["geometry"]['coordinates'][0], "]")
    
    # Vérification et conversion de la date
    if 'record_timestamp' not in contenu[i]:
        contenu[i]['record_timestamp'] = "-"
    else:
        iso_date = contenu[i]['record_timestamp'] # Récupère la date au format ISO
        dt = datetime.fromisoformat(iso_date) # Convertit en objet datetime
        contenu[i]['record_timestamp'] = dt.strftime("%d/%m/%Y %H:%M:%S") # Format pour Excel
    print("Date : ", contenu[i]['record_timestamp'])
    
    # Vérification et extraction du nom du polluant
    if 'nom_poll' not in contenu[i]['fields']:
        contenu[i]["fields"]['nom_poll'] = "-"
    print("Polluant : ", contenu[i]["fields"]['nom_poll'])
    
    # Vérification et extraction de la valeur mesurée
    if 'valeur' not in contenu[i]['fields']:
        contenu[i]["fields"]['valeur'] = "-"
    print("Valeur : ", contenu[i]["fields"]['valeur'])
    
    # Vérification et extraction de l'unité de mesure
    if 'unite' not in contenu[i]['fields']:
        contenu[i]["fields"]['unite'] = "-"
    print("Unité : ", contenu[i]["fields"]['unite'])
    
    # Création d'une liste avec tous les champs   
    champs = [
        contenu[i]["fields"]['nom_station'],
        contenu[i]["geometry"]['coordinates'][0],
        contenu[i]["geometry"]['coordinates'][1],
        contenu[i]['record_timestamp'],
        contenu[i]["fields"]['nom_poll'],
        contenu[i]["fields"]['valeur'],
        contenu[i]["fields"]['unite']
    ]
    # Garde seulement les lignes sans "-" (complètes)
    if "-" not in champs:
        LignesFiltrees.append(i)
    else:
        LignesInvalides += 1
   
# Affichage du résumé du traitement    
print("=" * 59)
print(" ✓ Le programme a traité : ", str(LongueurContenu), "lignes.", "\n", "✓ Fichier CSV créé avec succès !")
print(" ✘ On n'a pas pu traiter ", str(LignesInvalides), " lignes (étant incomplètes).")  

# Écriture du fichier CSV
try:
    fichier = open('fichier.csv', 'w', newline='', encoding = "utf-8-sig") # Ouvre le fichier CSV
    ecritCSV = csv.writer(fichier, delimiter=";") # Crée un objet writer avec délimiteur ";"
    # Écrit la ligne d'en-têtes
    ecritCSV.writerow(["Nom_Station","Latitude","longitude","Date de prélèvement", "Nom_Polluant", "Valeur", "Unité"])

    # Parcourt les lignes valides et écrit chaque ligne
    for i in LignesFiltrees:
        fichier.write(str (contenu[i]["fields"]['nom_station']) + ";")
        fichier.write(str (contenu[i]["geometry"]['coordinates'][1]) + ";") # Latitude
        fichier.write(str (contenu[i]["geometry"]['coordinates'][0]) + ";") # Longitude
        fichier.write(str (contenu[i]['record_timestamp']) + ";")
        fichier.write(str (contenu[i]["fields"]['nom_poll']) + ";")
        fichier.write(str (contenu[i]["fields"]['valeur']) + ";")
        fichier.write(str (contenu[i]["fields"]['unite']) + "\n") # Ajoute le retour à la ligne
        
    fichier.close() # Ferme le fichier
  
# Si il y a une erreur lors de l'écriture du CSV, l'utilisateur est prévenu
except Exception as e:
    print (" ✘ Une erreur est survenue lors de l'écriture du fichier CSV.")
    print(" ➯", e)      
    # Le type d'erreur est dans la variable e













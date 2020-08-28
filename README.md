# Mexican Congressional Debates 

This repo contains a `webcrawler` to scrape the Mexican Congressional Debates held in the **Cámara de Diputados** along with a  `structured database` that contains the interventions for each representative in the sessions. Also it is provided the a file that contains the metadata of each session (e.g the president of the Chamber, quorum). The following section contains a summary of the folders and files of this repo. 

Main source: http://cronica.diputados.gob.mx

## Project files

### Webcrawler: Using the scraper
The webcrawler is Python-based, for its use installation is necessary, as well as some Python libraries. 

* **source** - folder containing all the code to scrape as well as a tutorial to run the scraper
  * **main.py** - main code file that is used when running the program
  * **requierements.txt** - txt file with Python Libraries requiered
  * **legislaturas.txt** - txt file with Legislaturas that will be scraped when the program is run

### Data

The data is current as of **fecha de hoy**. There has been a post-processing of the data in comparison to the original sources. I recommend using the webcrawler if you need to use the original data without any pre-processing. 

* **data** - folder containing all the data and codebook. 
  * **metadata.csv** - csv file containing the metadata of each session
  * **congressional_debates.csv** - file containing the legislative debates at the level of session.
  * **representartive_speeches.csv** - csv file with legislative debates at the level of representative.

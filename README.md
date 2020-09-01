# MX Congressional Record

This repo provides a  `dataset that contains processed text` for recent sessions at **Cámaras de Diputados** of the Mexican Congress. It also includes a `webcrawler` (along with its tutorial), to scrape the raw data of each congressional session. Data has been retrieved from the Camara's website (http://cronica.diputados.gob.mx).  

The dataset includes all text spoken on the floor of each chamber of Congress: **Cámaras de Diputados** and **Cámara de Senadores**.  An automated script parses the text from each session to produce `full-text speeches`, `metadata on speeches,  their speakers and the session`. 

Last update: 2020-08-28

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
  * **representative_speeches.csv** - csv file with legislative debates at the level of representative.

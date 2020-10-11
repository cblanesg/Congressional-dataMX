** On Progress **
# MX Congressional Data

This repo provides a `dataset that contains legislative behavior measures of` the Chamber of Deputie's members of the Mexican Congress. This corpus contains five groups of files: (i) id data of members (including career track and participation in committees) (ii) floor speech data (interventions of each member in debates, metadata of debate, metadata of speaker) (iii) roll call votes, (v) legislative data (bills proposed, etc.) (iv) legislative effort (measure composed of previous variables). (v) Idealogy Positioning (ideal point estimations from roll call votes and from speech). For each group of file is also provided webscraping code. All the data was obtained from (http://www.diputados.gob.mx/sistema_legislativo_LXIV.html). 

`Terms: Legis LXV, LXI, LXII, LXIII, LXIV.`

Last update: 2020-10-11

## Project files

### Source:
All webcrawlers are Python-based (almost all webcrawlers use Selenium). 

* **source** - folder containing all the code to scrape group of files
  * **id_legislatura.py** - code to obtain members of each Legislatura along with some metadata (estado, distrito, type of member (SSD, RP))
  * **committe_members.py** - members' participation in commmittes
  * **gender_members.py** - members' gender (only Propietary)
  * **career_track.py** - career track of incumbents (includes previous educational formation, private and public experience and organizations that are part of. )
  * **floor_speech.py** - intervention on each member in all debate sessions. 
  * **debate.py** - full-text of debate
  * **floor_attendance.py** - floor attendance of members in each session
  * **roll_call-votes.py** - roll call votes in sessions
  * **bills_proposed.py** - members' bills proposed and status of bill (approved, declined, on process). 


### Data

The data is current as of **2020-09-24**. There has been a post-processing of the data in comparison to the original sources. I recommend using the webcrawler if you need to use the original data without any pre-processing (## indicates Legislatura)

* **data** - folder containing all the data and codebook. 
  * **id_legislatura##.json** - code to obtain members of each Legislatura along with some metadata (estado, distrito, type of member (SSD, RP))
  * **committe_members##.json** - members' participation in commmittes
  * **gender_members##.json** - members' gender (only Propietary)
  * **career_track##.json** - career track of incumbents (includes previous educational formation, private and public experience and organizations that are part of. )
  * **floor_speech##.json** - intervention on each member in all debate sessions. 
  * **debate##.json** - full-text of debate
  * **floor_attendance##.json** - floor attendance of members in each session
  * **roll_call-votes##.json** - roll call votes in sessions
  * **bills_proposed##.json** - members' bills proposed and status of bill (approved, declined, on process). 

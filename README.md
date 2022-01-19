Bike Ride Data
The files in this repository are for a case study project for the Google Data Analytics Certificate.
The case study is one based upon actual data from a bike share company in Chicago, but treated as part of a fictional company's request for analysis (in the Certificate).
The original 12 csv files that formed the basis of the RData file in this repository are found at:
https://divvy-tripdata.s3.amazonaws.com/index.html
The specific files downloaded were those from November, 2020 to October, 2021.
After downloading and opening these files, the specific actions taken in Microsoft Excel were:

Saved each as xlsx file type with prefix ‘new_’ added before year
Autofit column width on columns C and D
Insert 5 new columns E-I; in E1, title it ride_length
In E2, type =$D2 - $C2
Fill the column
Set column format to Time 37:30:55
In F1, type ride_length_seconds
In F2, type 

=(HOUR($E2) + INT($E2)*24)*3600 + MINUTE($E2)*60 + SECOND($E2)

Fill column, change format to Number with 0 decimal places
Copy Column F, paste values and number formatting into G1
Delete columns E and F
In now blank column F, label F1 as weekday, type into F2 =weekday($C2)
Fill the column
Format column F as General, such that Sunday is 1 and Saturday is 7
Copy column F; click in Empty G1, paste values and number formatting
Delete column F
Save each file as Year_Month_Bikes_Final.xlsx
Also save each file as Year_Month_Bikes_Final.csv
Create zip file of all 12 months, name it Bike_Data_Files_CSV.zip

Once the zip file was created, I opened up RStudio and created a new script.
That script is in this repository, and contains all elements of Data Cleaning, Manipulation, Analysis, and Visualization that I performed.
The RData file in this repository is the finished dataframe containing the combined and cleaned ~5.4 million rows sourced from the 12 csv files.
A mock PowerPoint presentation can be found on my website, under the Bike Share Project Header, at:
mitchellcron.wordpress.com

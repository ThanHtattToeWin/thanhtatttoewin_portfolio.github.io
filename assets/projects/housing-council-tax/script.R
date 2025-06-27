# To install the RSQLite package 
install.packages("RSQLite")

# To load the RSQLite library
library(RSQLite)

# To set the working directory to the folder where the database is stored
setwd("F:/Data Analytics/Coursework/Couserwork1_files/")

# To connect the SQLite database
conn <- dbConnect(SQLite(), "database.db")

# To check if the connection is valid
if (dbIsValid(conn)) {
  print("Connection to the database is successful!")
} else {
  print("Connection to the database failed.")
}

# First query: To calculate average house price in 2 years for a given ward in a particular district
query_avg_price <- " 
SELECT 
    w.Ward_Name,
    la.Local_Authority_Name,
    AVG(m.Median_Price) AS Average_Price    -- Aggregate function AVG() is used to find the average
FROM  
    MedianHousePrice m
INNER JOIN 
    Ward w ON m.Ward_Code = w.Ward_Code
INNER JOIN 
    LocalAuthority la ON w.Local_Authority_Code = la.Local_Authority_Code
WHERE 	
    (m.Year_Ending LIKE 'Mar-21' OR m.Year_Ending LIKE 'Mar-22' OR m.Year_Ending LIKE 'Jun-21' 
    OR m.Year_Ending LIKE 'Jun-22' OR m.Year_Ending LIKE 'Sep-21' OR m.Year_Ending LIKE 'Sep-22' 
    OR m.Year_Ending LIKE 'Dec-21' OR m.Year_Ending LIKE 'Dec-22')      -- Quaters of two years (2021, 2022)
AND 
    w.Ward_Code = 'E05010922'       --Can be replaced with the desired Ward_Code
GROUP BY  
    w.Ward_Name, la.Local_Authority_Name;
"
# Executing the query and storing the result
result_avg_price <- dbGetQuery(conn, query_avg_price)

# To print result for the average house price for a given ward 
print(result_avg_price)



# Second query: To find ward with highest house price in a particular quater of year
query_highest_price <- "
SELECT 
    w.Ward_Name,
    la.Local_Authority_Name,
    MAX(m.Median_Price) AS Highest_Price  -- Aggregate function MAX() is used to find the highest
FROM  
    MedianHousePrice m
INNER JOIN 
    Ward w ON m.Ward_Code = w.Ward_Code
INNER JOIN 
    LocalAuthority la ON w.Local_Authority_Code = la.Local_Authority_Code
WHERE 
    m.Year_Ending = 'Mar-21'        --  Can be replaced with desired year/quarter
GROUP BY 
    w.Ward_Name, la.Local_Authority_Name
ORDER BY 
    Highest_Price DESC   -- Order by house price in descending order
LIMIT 1;                  -- Get the highest one of the price
"
# Executing the query and storing the result in a variable
result_highest_price <- dbGetQuery(conn, query_highest_price)

# To print result for ward of the highest house price in the specified quarter 
print(result_highest_price)



# Third query: To find average council tax charge for a particular town in a particular district 
# for any three bands of properties.
query_avg_tax <- "
SELECT 
    l.Location_Name, 
    la.Local_Authority_Name, 
    SUM(c.CouncilTax) / 3 AS Average_Council_Tax_for_Any_Three_Bands 
    -- SUM() is used to sum total 3 council tax
FROM 
    CouncilTax c
INNER JOIN
    Location l ON c.Location_ID = l.Location_ID
INNER JOIN 
    LocalAuthority la ON l.Local_Authority_Code = la.Local_Authority_Code
WHERE 
    l.Location_Name = 'Banbury' 
    AND l.Local_Authority_Code = 'E07000177'   -- can be replaced with desired authority code
    AND c.Band IN ('A', 'B', 'C');        --- can be replaced with any three bands
"
# Executing the query and storing the result in a variable
result_avg_tax <- dbGetQuery(conn, query_avg_tax)

# To print result for average tax with ward and district name
print(result_avg_tax)



# Fourth query: To calculate difference between council tax charges of same bands
# but two different wards in same district
compare_council_tax_query <- "
SELECT 
    loc1.Location_Name AS Town_1,
    loc2.Location_Name AS Town_2,
    ct1.Band AS Band,
    (ct1.CouncilTax - ct2.CouncilTax) AS Tax_Difference -- difference between council tax charges
FROM 
    CouncilTax ct1
INNER JOIN 
    Location loc1 ON ct1.Location_ID = loc1.Location_ID
INNER JOIN 
    CouncilTax ct2 ON ct1.Band = ct2.Band
INNER JOIN 
    Location loc2 ON ct2.Location_ID = loc2.Location_ID
WHERE 
    loc1.Local_Authority_Code = loc2.Local_Authority_Code   -- Same district
    AND loc1.Location_Name = 'Adderbury' -- First fixed town(Can be replaced with desired ward of same district)
    AND loc2.Location_Name = 'Ambrosden' -- Second fixed town(Can be replaced with desired ward of same district)
    AND ct1.Band = ct2.Band  -- Same band
ORDER BY 
    ct1.Band;
"
# Executing the SQL query and fetching the results
council_tax_comparison <- dbGetQuery(conn, compare_council_tax_query)

# To print result for fourth query
print(council_tax_comparison)



# Fifth query: To find the ward of lowest council tax for band B properties
lowest_council_tax_query <- "
SELECT 
    loc.Location_Name AS Town,
    ct.Band,
    ct.CouncilTax
FROM 
    CouncilTax ct
INNER JOIN 
    Location loc ON ct.Location_ID = loc.Location_ID
WHERE 
    loc.Local_Authority_Code = 'E07000177'  -- Cherwell district code
    AND ct.Band = 'B' -- Band B properties
ORDER BY 
    ct.CouncilTax ASC     -- Order by council tax in ascending order
LIMIT 1; -- Get the town with the lowest council tax
"

# To execute the SQL query and fetch the result
lowest_council_tax_result <- dbGetQuery(conn, lowest_council_tax_query)

# To print the result for fifth query
print(lowest_council_tax_result)

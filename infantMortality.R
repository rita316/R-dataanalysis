
### Extract infant mortality from a XML file and use Google Earth for data visualization. 

### Part 1.  Create the data frame from XML file

### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### (a) Load the data frame called LatLon from hw7.rda. 
load('hw7.rda')
library(XML)
### (b) Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 
factbook =  xmlParse('factbook.xml')
factbook_root =  xmlRoot(factbook)
factbook_root
### (c) Use XPath to extract the infant mortality and the CIA country codes from the XML tree
infantnode = getNodeSet(factbook,'//field[@name="Infant mortality rate"]/rank')
infantnode
infantmort =  as.numeric(sapply( infantnode, xmlGetAttr, 'number'))
datelatest=  (sapply( infantnode, xmlGetAttr, 'dateLatest'))
datelatest
infantmort
CIA.Codes = sapply(infantnode, xmlGetAttr, 'country') 
CIA.Codes
### (d) Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.
IM =  data.frame(infantmort, CIA.Codes)
names(IM) = c("Infant Mortality","CIA.Codes")

### (e) Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
popnode = getNodeSet(factbook_root, '//field[@name="Population"]/rank')
popnode
population = as.numeric(sapply(popnode, xmlGetAttr, 'number'))
population
CIA.Codes2 = sapply(popnode, xmlGetAttr, 'country')
CIA.Codes2

Pop = data.frame(population, CIA.Codes2)
names(Pop) = c('Population', "CIA.Codes")
### (f) Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMpop = merge(IM, Pop, by = "CIA.Codes")
IMpop$CIA.Codes =  toupper(IMpop$CIA.Codes)  
### (g) Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)
alldata =  merge(IMpop, LatLon, by = "CIA.Codes")
alldata = alldata[,c(5,6,1,4,3,2)]


### Part 2.  Create a KML document for google earth visualization.
### Make the KML document with stucture described in hw7_Intro.pdf.  You can use the addPlacemark function below to make
### the Placemark nodes, for which you need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
### This code creates the template for KML document 
### Your code here
    doc = newXMLDoc()
    rootNode = newXMLNode('kml', parent = doc)
    DocNode = newXMLNode('Document', parent = rootNode)
    
    newXMLNode("Name", "Country Facts", parent= DocNode)
    
    newXMLNode("Description", "Infant Motality", parent = DocNode)

    LookAtNode = newXMLNode("LookAt", parent = DocNode)
    newXMLNode("longtitude", "-121", parent = LookAtNode)
    newXMLNode("latitude", "43", parent = LookAtNode)
    newXMLNode("altitude", "4100000", parent = LookAtNode)
    newXMLNode("title", "0",parent = LookAtNode)
    newXMLNode("heading", "0",parent = LookAtNode)
    newXMLNode("altitudeMode", "absolute",parent = LookAtNode)
    
    FolderNode = newXMLNode("Folder", parent = DocNode)
    newXMLNode('Name', parent = FolderNode)
    

    return(doc)
}

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

  newXMLNode("Point" ### Your code here)
             
### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}


### Use the two functions that you just implemented to created the KML document and save it 
### as 'Part2.kml'. open it in Google Earth. (You will need to install Google Earth.)  
### It should have pushpins for all the countries.  

### Your code here



### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier. To be more specific, instead of pushpins, we
### want different circle labels for countris with size representing population and the color representing  
### the infant motality rate.
### Pretty much all the code is given to you below to create style elements.
### Here, you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
infCut = cut(someNumericalVector, breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(someOtherNumericalVector, breaks = 5)
popCut = as.numeric(popCut)

### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE
### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to use for the sizes of your circles. Try different 
### setting of scale here.

# scale = c(XX,XX,XX,XX,XX) Try your scale here for better visualization
colors = c("blue","green","yellow","orange","red")

addStyle = function(col1, pop1, parent, DirBase, scales = scale)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
             newXMLNode("scale", scales[pop1]), 
             newXMLNode("Icon", paste(DirBase, "color_label_circle_", colors[col1], ".png", sep ="")), parent = st)
}


root2 = xmlRoot(doc2)
DocNode = root2[["Document"]]


for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, DocNode, 'color_label_circle/')
  }
}

### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files
### Your code here



### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded hw7.rda.


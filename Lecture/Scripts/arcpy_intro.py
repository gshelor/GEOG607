import arcpy # Imports the arcpy module
import os # Imports the os module, helping with workspaces and read/write

######### Set up workspace and environment settings ######

#allows you to overwrite existing files
arcpy.env.overwriteOutput = True 

# Points to working folder
arcpy.env.workspace = "C:/Users/sbkel/Dropbox/UNR_Teaching/GEOG_407_607/Demos/ArcPyDemo/arcpyTests"

# Set output folder for exports/intermediary files
outWorkspace = "C:/Users/sbkel/Dropbox/UNR_Teaching/GEOG_407_607/Demos/ArcPyDemo/IntermediaryOutputs"

#########

# Read input data
FourCorners = "FourCorners.shp"
Counties = "tl_2016_us_county.shp"
RenoRoads = "RenoRoads.shp"

# Get the spatial reference of input data
getinfo = arcpy.Describe(FourCorners)
spatialref = getinfo.spatialReference
print (spatialref.name)

# Or, get all at once through a definite loop - then reproject!
for i in arcpy.ListFeatureClasses():

    # The describe function is helpful to get at elements of the feature,
    # including fields, projection info, data type, etc. Here, we set as a variable
    desc = arcpy.Describe(i)

    # Check to make sure all features have a defined coordinate system
    if desc.spatialReference.Name == "Unknown":
        print ('Coordinate System Undefined. Please use Define Projection tool for' + i)
    else:
        # Set output feature - will save in workspace with name from input
        projfc = os.path.join(outWorkspace, i)
        # Assign a new Spatial Reference variable
        newCS = arcpy.SpatialReference(102003)
        # Execute the project tool
        arcpy.Project_management(i,projfc,newCS)
        # Report progress as you would see in ArcGIS
        print (arcpy.GetMessages())

# Buffering - an example of GIS analysis in ArcPy

bufferoutput = os.path.join(outWorkspace,"RenoRoadsBuffer") 
arcpy.Buffer_analysis(RenoRoads, bufferoutput, "1320 feet","FULL","ROUND","ALL","")
print (arcpy.GetMessages())

# Get at field names in file
fields = arcpy.ListFields(Counties)
for i in fields:
    print (i.name)

# Using a search cursor to move through rows of an attribute table
rows = arcpy.SearchCursor(FourCorners)

namelist = []
for i in rows:
    SWCounties = i.getValue("NAME")
    namelist.append(SWCounties)
print (namelist)

# Let's add a field to the Reno Roads Buffer to calculate area for buffer(s)
arcpy.AddField_management("C:/Users/sbkel/Dropbox/UNR_Teaching/GEOG_407_607/Demos/ArcPyDemo/IntermediaryOutputs/RenoRoadsBuffer.shp", "Square_Mi","FLOAT","","","")
arcpy.AddGeometryAttributes_management("C:/Users/sbkel/Dropbox/UNR_Teaching/GEOG_407_607/Demos/ArcPyDemo/IntermediaryOutputs/RenoRoadsBuffer.shp","AREA","","SQUARE_MILES_US")

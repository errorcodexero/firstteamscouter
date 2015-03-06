import os

#rootDir = "D:\\Profiles\\TomS\\Documents\\FIRST Robotics\\2014\\First Team Scouter\\tablet data files"
#rootDir = "C:\\Users\\Tom\\Documents\\FIRST Robotics\\2014\\First Team Scouter\\Corvallis Data"
rootDir = "D:\\Profiles\\TomS\\Documents\\FIRST Robotics\\2014\\Portland Regional Championship"

dataFile = "PortlandMasterData.csv"

filesToParse = []
fileExt = '.csv'

entries = os.listdir(rootDir)
print entries

for entry in entries :
    path = rootDir + "\\" + entry
    if os.path.isdir(path) :
        print "Directory: " + path
    elif os.path.isfile(path) and path.endswith(fileExt) and not path.endswith(dataFile) :
        #print "File: " + path
        filesToParse.append(path)
    else :
        print "/shrug: " + path

#print "About to parse the following files: "
#print csvFiles

lines = []
filesParsed = 0
for csvFile in filesToParse :
    with open(csvFile) as f :
        lineCount = 0
        for line in f :
            if lineCount > 0 :
                lines.append(line.strip())
            lineCount += 1
    filesParsed += 1
    pathArray = csvFile.split("\\")
    #path = "\\".join(pathArray[0:-1])
    #savePath = path + "\\saved"
    savePath = rootDir + "\\saved"
    if not os.path.isdir(savePath) :
       os.mkdir(savePath)
    numSaveFiles = len(os.listdir(savePath)) + 1
    saveFile = savePath + "\\" + str(numSaveFiles) + "_" + pathArray[-1]
    try :
        os.rename(csvFile, saveFile)
    except WindowsError as e :
        print "Windows Error({0}): {1}".format(e.errno, e.strerror)
        print csvFile
        print saveFile

outFilePath = rootDir + "\\" + dataFile
outFile = open(outFilePath, 'a')

print "Parsed " + str(filesParsed) + " csv files\n"
for line in lines:
    outFile.write(line.strip() + "\n")
outFile.close()






    

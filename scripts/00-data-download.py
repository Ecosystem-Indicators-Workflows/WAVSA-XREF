import requests, json
import pyprojroot
import os
from pathlib import Path
from dotenv import load_dotenv

repodir = pyprojroot.find_root(pyprojroot.has_dir(".git"))
output_dir = repodir / 'data' / 'source-data' 
Path(output_dir).mkdir(parents=True, exist_ok=True)


load_dotenv(repodir / ".env_local")  

userId=os.getenv("SLIP_USER")
password = os.getenv("SLIP_PWD")
folderName="DPIRD_Public"
fileName="Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public_Geopackage.zip"

tokenRequestUrl = "https://sso.slip.wa.gov.au/as/token.oauth2"
tokenRequestHeaders = { 'Authorization' : 'Basic ZGlyZWN0LWRvd25sb2Fk'}
tokenRequestForm={"grant_type": "password", "username":userId, "password":password}
tokenResponse = requests.post(tokenRequestUrl, data=tokenRequestForm, headers=tokenRequestHeaders)
accessToken=json.loads(tokenResponse.text)["access_token"]
if tokenResponse.status_code == 200:
    dataDownloadRequestUrl = "https://direct-download.slip.wa.gov.au/datadownload/{0}/{1}".format(folderName, fileName)
    print("Downloading file from URL: " + dataDownloadRequestUrl)
    dataDownloadRequestHeaders = { 'Authorization' : 'Bearer ' + accessToken}
    dataDownloadResponse = requests.get(dataDownloadRequestUrl, headers=dataDownloadRequestHeaders)
    if dataDownloadResponse.status_code == 200:
        with open(output_dir / fileName, 'wb') as f:
            f.write(dataDownloadResponse.content)
    else:
        print("Error download file with error " + str(dataDownloadResponse.status_code) + "-" + dataDownloadResponse.text)
else:
    print("Error getting token: " + str(tokenResponse.status_code) + "-" + tokenResponse.text)
Small overview over the conversion of the raw data file to a root file.

converttoroot.py is running bash scripts inside the jade software container, which needs to have: 

- the name jade_soft
- the built JADE directory mounted in the home directory of the container

in order to run properly.
This can be done by running the image with 
"docker run -dit --platform linux/amd64 --name jade_soft -v JADE_FOLDER_PATH:/home ghcr.io/andriish/fedora39x86_64i686_gnu"
and replacing JADE_FOLDER_PATH with the path to the JADE repository.

The python script has to be run in place.
# cmpdir
Compare the files in two directories (recursive). First Use Case is to compare two iTunes Libraries.


# iTunes and Sonos

## iTunes/iTunes Media/Music

$ find . -type f -mmin -5
./iTunes/iTunes Music Library.xml


./iTunes/iTunes Music Library.xml contains:

        <key>Music Folder</key>
        <string>file:///Users/gpaulissen/Music/iTunes/iTunes%20Media/</string>

## iTunes/iTunes Media/Music.bak

Unix: remove xml

Sonos: Update Music Library now

$ find . -type f -mmin -5
./iTunes/iTunes Music Library.xml

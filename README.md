# mgit
## Multi repo git

## Dev notes
### Install libs:

brew install icu4c

stack install text-icu \
 --extra-lib-dirs=/usr/local/opt/icu4c/lib \
 --extra-include-dirs=/usr/local/opt/icu4c/include
 
stack install yi (optional)
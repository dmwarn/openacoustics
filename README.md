# openacoustics
Code to assist with reproducible processing of hydroacoustic data.

The package can be installed in R using the command devtools::install_github('dmwarn/openacoustics'). Documentation of the functions is currently relatively poor. There are complications associated with using the package related to the required RDCOMClient package. This package does not work natively with R versions > 3.6 or 3.7. There is a workaround that I have borrowed from https://stackoverflow.com/questions/61735315/cant-build-rdcomclient-using-rtools40-and-r-4-0/62658906#62658906. It involves having to versions of Rtools installed and present in your system path with one being Rtools 3.5, I think. 



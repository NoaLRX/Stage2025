#!/bin/bash
cp app.R app.R.temp
sed -e "633,723d" app.R.temp > app.R.fixed
mv app.R.fixed app.R

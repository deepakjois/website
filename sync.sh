#!/bin/sh
# rsync -dave ssh site/ root@deepak.jois.name:/var/www/
s3cmd sync  -r ./site/*  s3://vyom-site


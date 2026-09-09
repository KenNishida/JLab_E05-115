#!/bin/sh

root -b <<EOF
 .x check.C($1) 
EOF
acroread run$1.pdf

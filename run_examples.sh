#!/bin/bash

for file in ./examples/examples/examples/*.js; do
  node "$file"
done

[![Build Status](https://travis-ci.org/jbryl7/SLang.svg?branch=master)](https://travis-ci.org/jbryl7/SLang)
# SLang



With sbt preinstalled:
- run tests:
> sbt "test"
- run program with input file
> sbt "run path_to_input_file"



If you do not want to install sbt:
- build image:
> docker build -t slang .
- run tests:
> docker run -it slang sbt "test"
- run program with input file
> docker run -it slang sbt "run path_to_input_file"

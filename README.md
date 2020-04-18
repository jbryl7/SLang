[![Build Status](https://travis-ci.org/jbryl7/SLang.svg?branch=master)](https://travis-ci.org/jbryl7/SLang)
# SLang

If you do not want to install sbt but you have docker:
- 1 build image:
> docker build -t slang .
- 2 run sbt:
> docker run -it slang sbt
- 3 look at section "with sbt preinstalled"

With sbt preinstalled:
- 1 run sbt:
> sbt
- 2.1 run tests:
> test
- 2.2 run program with input file
> run path_to_input_file

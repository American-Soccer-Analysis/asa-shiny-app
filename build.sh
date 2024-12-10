#!/bin/sh

docker stop asa-shiny-app
docker rm asa-shiny-app
docker build . -t asa-shiny-app:latest
docker run -it -p 3838:3838 --cpus 1 --memory 1g --name asa-shiny-app asa-shiny-app:latest
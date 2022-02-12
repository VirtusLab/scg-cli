docker build -t liosedhel/scg-jupyter:0.0.3 .

docker run -d -p 8888:8888 -p 4040:4040 -p 4041:4041 -v /Users/kborowski/phd/metals:/home/jovyan/data --name scg-jupyter liosedhel/scg-jupyter:0.0.3


NAME=mavis
TAG=$(shell git tag -l | tail -n 1)
DIR=/tmp/${NAME}-${TAG}

pack:
	rm -rf ${DIR}
	mkdir -p ${DIR}/prolog
	rm -f prolog/*~
	cp pack.pl ${DIR}
	cp -R prolog ${DIR}
	tar -czv -C /tmp -f ${NAME}-${TAG}.tgz ${NAME}-${TAG}

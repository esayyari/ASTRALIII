#!/bin/bash
# Experiment2 input and command; require nw_ed
DIR=/home/chaos/repository/Experiment2/
JAR=/home/chaos/repository/ver3.jar
JAR2=/home/chaos/repository/ver2.jar

rm command
for j in avian-0_5X-1000-500-all.f200 avian-1X-1000-500-all.f200 avian-2X-1000-500-all.f200 avian-1X-1000-1500-all.f200 avian-1X-1000-1000-all.f200 avian-1X-1000-250-all.f200
do
	for k in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
	do
		head -${k}000 $j > temp
		tail -1000 temp > $j.$k.ori
		echo "java -jar ${JAR} -t 0 -i ${DIR}$j.$k.ori -o ${DIR}$j.$k.ori.out3 &> ${DIR}$j.$k.ori.log3" >> command
		for i in 0 3 5 7 10 20 33 50 75
		do
			nw_ed $j.$k.ori "i & b<=$i" o > $j.$k.t$i
			echo "java -jar ${JAR} -t 0 -i ${DIR}$j.$k.t$i -o ${DIR}$j.$k.t$i.out3 &> ${DIR}$j.$k.t$i.log3" >> command
		done
	done
done
for j in mammalian-0_5X-200-500-all.f200 mammalian-1X-200-500-all.f200 mammalian-1X-200-1000-all.f200 mammalian-2X-200-500-all.f200
do
	for k in 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40
	do
		head -${k}00 $j > temp
		tail -200 temp > $j.$k.ori
		echo "java -jar ${JAR} -t 0 -i ${DIR}$j.$k.ori -o ${DIR}$j.$k.ori.out3 &> ${DIR}$j.$k.ori.log3" >> command
		for i in 0 3 5 7 10 20 33 50 75
		do
			nw_ed $j.$k.ori "i & b<=$i" o > $j.$k.t$i
			echo "java -jar ${JAR} -t 0 -i ${DIR}$j.$k.t$i -o ${DIR}$j.$k.t$i.out3 &> ${DIR}$j.$k.t$i.log3" >> command
		done
	done
done
for j in avian-0_5X-1000-500-all.f200
do
	for k in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
	do
		echo "java -jar ${JAR2} -t 0 -i ${DIR}$j.$k.ori -o ${DIR}$j.$k.ori.out2 &> ${DIR}$j.$k.ori.log2" >> command
		for i in 0 3 5 7 10 20 33 50 75
		do
			echo "java -jar ${JAR2} -t 0 -i ${DIR}$j.$k.t$i -o ${DIR}$j.$k.t$i.out2 &> ${DIR}$j.$k.t$i.log2" >> command
		done
	done
done

#! /bin/bash

for f in inp/*Obs-*
do
	dist/build/filter-obs/filter-obs "$f" >"outp/${f:4}-filtered.txt" 2>"log/${f:4}-log.txt" &
done

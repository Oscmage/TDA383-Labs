all: src/*.java
	javac -sourcepath src -d bin src/Main.java

run: all
	java -cp bin Main Lab2.map 5 15

clean:
	rm -rf bin/*
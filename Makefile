exe:
	lein compile
	lein uberjar
clean:
	rm target/*.jar

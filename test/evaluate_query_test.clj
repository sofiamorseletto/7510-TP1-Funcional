(ns evaluate-query-test
	(:require [clojure.test :refer :all]
		[logical-interpreter :refer :all]
		[clojure.string :as str]))

(def database "
	varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")

(def entradaParseada (parsear database))
(def facts (procesarFacts entradaParseada))
(def rules (procesarRules entradaParseada))

(def queryParseada (parsearFact "hijo(pepe, juan)"))
(def argumentos (str/split (nth queryParseada 3) #",[\s]?"))
(def referencias (nth (get rules (nth queryParseada 1)) 1))
(def mapaRefArg (zipmap referencias argumentos))
(def patron (str/join "|" referencias))

(deftest keywords-replacement-test
	(testing "hijo(X, Y) should be replaced for hijo(pepe, juan)"
		(is (= (reemplazarValores patron mapaRefArg ["hijo(X, Y)" "hijo(X, Y)"]) "hijo(pepe, juan)"))))

(deftest facts-map-creation
	(testing "A map structure should be created with 'varon' as key and 'juan' as value"
	 (is (= (crearMapFacts ["\tvaron(juan)." "varon" "(juan)" "juan"])
	  {"varon" ["juan"]})))
	(testing "A map structure should be created with 'padre' as key and 
	 'juan, pepe' as value"
	 (is (= (crearMapFacts ["\tpadre(juan, pepe)." "padre" "(juan, pepe)" "juan, pepe"])
	  {"padre" ["juan, pepe"]}))))

(deftest rule-processing
	(testing "A map structure should be created with the name of the rule as key
	 and its facts and arguments as value"
	 (is (= (procesarRules entradaParseada) 
	 	{"hijo" ["varon(X), padre(Y, X)." ["X" "Y"]], "hija" ["mujer(X), padre(Y, X)." ["X" "Y"]]}))))

(deftest search-rule
	(testing "hija(maria, hector) should return true"
		(is (= (buscarRule rules facts (parsearFact "hija(maria, hector)")) true))))
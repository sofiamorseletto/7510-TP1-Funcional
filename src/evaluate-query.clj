(ns logical-interpreter
	(:require [clojure.string :as str]))

(defn parsear [text]
	(str/split text #"\n"))

(defn parsearFact [text]
	(re-matches #"[\s]*?([a-zA-Z]*)(\((.*[\w\s,$]*)\)).?" text))

(defn parsearRule [text]
	(re-matches #"[\s]*?([a-zA-Z]*)(\((.*[\w\s,$]*)\)) :- (.*)" text))

(defn crearMapFacts [entrada]
	(if (not= entrada nil) (assoc {} (nth entrada 1) [(nth entrada 3)])))

(defn crearMapRules [entrada]
	(if (not= entrada nil) (assoc {} (nth entrada 1) [(nth entrada 4) 
		(str/split (nth entrada 3) #",[\s]?")])))

(defn mapearRules [entrada]
	(crearMapRules (parsearRule entrada)))

(defn procesarFacts [entrada]
	(let [soloFacts (remove #(re-matches #"(.*) :- (.*)" %) entrada)
		factsParseadas (map parsearFact soloFacts)
		cantNil (get (frequencies factsParseadas) nil)]
	(if (> cantNil 1) nil (reduce #(merge-with into %1 %2) 
		(map crearMapFacts (remove nil? factsParseadas))))))

(defn procesarRules [entrada]
	(let [soloRules (filter #(re-matches #"[\s]*?(.*) :- (.*)" %) entrada)]
		(if (= (count soloRules) 0) nil (reduce #(merge-with into %1 %2) (map mapearRules soloRules)))))

(defn reemplazarValores [patron, mapaRefArg, fact]
	(str/replace (nth fact 1) (re-pattern patron) mapaRefArg))

(defn buscarFact [database, queryParseada]
	(let [clave (nth queryParseada 1)
		valor (nth queryParseada 3)]
		(some #(= valor %) (get database clave))))

(defn buscarRule [databaseRules, databaseFacts, queryParseada]
	(let [clave (nth queryParseada 1)
		argumentos (str/split (nth queryParseada 3) #",[\s]?")
		facts (nth (get databaseRules clave) 0)
		referencias (nth (get databaseRules clave) 1)
		mapaRefArg (zipmap referencias argumentos)
		patron (str/join "|" referencias)]
		(if (not= facts nil) (every? true? (map #(buscarFact databaseFacts (parsearFact %)) 
			(map #(reemplazarValores patron mapaRefArg %)
				(re-seq #"[\s]?([\sa-zA-Z]*\([\sa-zA-Z,]*\))" facts)))) false)))

(defn evaluate-query [database, query]
	(let [entradaParseada (parsear database)
		facts (procesarFacts entradaParseada)
		rules (procesarRules entradaParseada)
		queryParseada (parsearFact query)]
		(if (or (= queryParseada nil) (= facts nil)) nil (if (or 
			(= (buscarRule rules facts queryParseada) true) (= (buscarFact facts queryParseada) true)) true false))))


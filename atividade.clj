(ns integrador.core
  (:require [clojure.string :as str])
  (:gen-class))

;; === Funções utilitárias ===

(defn ler [mensagem]
  (print mensagem)
  (flush)
  (read-line))

(defn ler-nota []
  (try
    (Double/parseDouble (ler "Digite a nota: "))
    (catch Exception _
      (println "Entrada inválida! Digite um número válido.")
      (recur))))

(defn media [notas]
  (/ (reduce + notas) (count notas)))

(defn calcular-status [aluno]
  (assoc aluno :status (if (>= (:nota aluno) 6.0) "Aprovado" "Reprovado")))

;; === Funcionalidades principais ===

(defn cadastrar-alunos []
  (loop [alunos []]
    (let [nome (ler "Digite o nome do aluno (ou ENTER para sair): ")]
      (if (empty? nome)
        alunos
        (let [nota (ler-nota)]
          (recur (conj alunos {:nome nome :nota nota})))))))

(defn relatorio-notas [alunos]
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado!")
    (do
      (println "\n=== RELATÓRIO DE NOTAS ===")
      (let [alunos-status (map calcular-status alunos)
            media-geral (media (map :nota alunos))]
        (doseq [a alunos-status]
          (println (:nome a) "- Nota:" (format "%.2f" (:nota a)) "- Status:" (:status a)))
        (println "\nMédia geral da turma:" (format "%.2f" media-geral))))))

(defn estatisticas [alunos]
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado!")
    (let [alunos-status (map calcular-status alunos)
          total (count alunos-status)
          aprovados (count (filter #(= (:status %) "Aprovado") alunos-status))
          reprovados (- total aprovados)
          notas (map :nota alunos-status)
          maior (apply max notas)
          menor (apply min notas)
          media-geral (media notas)]
      (println "\n=== ESTATÍSTICAS GERAIS ===")
      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" (format "%.2f" maior))
      (println "Menor nota:" (format "%.2f" menor))
      (println "Média geral:" (format "%.2f" media-geral)))))


(defn buscar-aluno [alunos]
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado!")
    (let [alunos-status (map calcular-status alunos)
          nome (ler "Digite o nome do aluno a buscar: ")
          aluno (first (filter #(= (str/lower-case (:nome %))
                                   (str/lower-case nome))
                               alunos-status))]
      (if aluno
        (println "Aluno:" (:nome aluno)
                 "- Nota:" (format "%.2f" (:nota aluno))
                 "- Status:" (:status aluno))
        (println "Aluno não encontrado!")))))

;; === Função principal ===

(defn -main []
  (loop [alunos []]
    (println "\n=== MENU PRINCIPAL ===")
    (println "1 - Cadastrar Alunos")
    (println "2 - Relatório de Notas")
    (println "3 - Estatísticas Gerais")
    (println "4 - Buscar Aluno pelo Nome")
    (println "0 - Sair")
    (println "---------------------------")
    (let [opcao (ler "Escolha uma opção: ")]
      (cond
        (= opcao "1") (recur (cadastrar-alunos))
        (= opcao "2") (do (relatorio-notas alunos) (recur alunos))
        (= opcao "3") (do (estatisticas alunos) (recur alunos))
        (= opcao "4") (do (buscar-aluno alunos) (recur alunos))
        (= opcao "0") (println "Encerrando o sistema...")
        :else (do (println "Opção inválida!") (recur alunos))))))


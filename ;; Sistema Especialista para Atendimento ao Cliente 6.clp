(deftemplate problema
   (slot id)
   (slot nome)
   (slot solucao))

;; Configuração inicial de problemas conhecidos
(deffacts problemas-conhecidos
   (problema (id 1) (nome "Sem sinal") (solucao "Verificar se o aparelho está ligado e em uma área com cobertura"))
   (problema (id 2) (nome "Internet lenta") (solucao "Reiniciar o modem e verificar a quantidade de dispositivos conectados."))
   (problema (id 3) (nome "Fatura incorreta") (solucao "Checar a fatura no site ou aplicativo e entrar em contato com suporte financeiro."))
   (problema (id 4) (nome "Demora no atendimento") (solucao "Tente entrar em contato em outro horário."))
   (problema (id 5) (nome "Planos incorretos") (solucao "Revise os detalhes do seu plano e solicite ajuste com a central de atendimento")))

;; Template para entrada do usuário
(deftemplate problema-usuario
   (slot nome-usuario)
   (slot tipo))

;; Template para confirmar se o problema foi resolvido
(deftemplate confirmacao-usuario
   (slot resolvido))

;; Regra de boas-vindas
(defrule boas-vindas
   =>
   (printout t "***************************************" crlf
              " * Bem-vindo ao suporte da CyberTech *" crlf
              "***************************************" crlf crlf))

;; Regra para solicitar o nome do usuário e o tipo de problema
(defrule solicitar-dados-usuario
   =>
   (printout t "Por favor, digite seu nome: ")
   (bind ?nome (read))
   (printout t crlf "Selecione o número correspondente ao problema:" crlf
              "1 - Sem sinal" crlf
              "2 - Internet lenta" crlf
              "3 - Fatura incorreta" crlf
              "4 - Demora no atendimento" crlf
              "5 - Planos incorretos" crlf
              "Digite um número entre 1 e 5: ")
   (bind ?entrada (read))
   (while (or (not (integerp ?entrada)) (not (member$ ?entrada (create$ 1 2 3 4 5))))
      do
      (printout t "Entrada inválida. Digite um número entre 1 e 5: ")
      (bind ?entrada (read)))
   (assert (problema-usuario (nome-usuario ?nome) (tipo ?entrada))))

;; Regra para sugerir uma solução para o usuário
(defrule recomendar-solucao
   ?usuario <- (problema-usuario (nome-usuario ?nome) (tipo ?tipo))
   ?problema <- (problema (id ?tipo) (nome ?nome-problema) (solucao ?solucao))
   =>
   (printout t crlf "Olá, " ?nome "!" crlf
              "Problema ID: " ?tipo crlf
              "Nome: " ?nome-problema crlf
              "Solução: " ?solucao crlf)
   (printout t "O problema foi resolvido? (sim/nao): ")
   (bind ?resposta (read))
   (while (not (member$ ?resposta (create$ sim nao)))
      do
      (printout t "Entrada inválida. Digite 'sim' ou 'nao': ")
      (bind ?resposta (read)))
   (assert (confirmacao-usuario (resolvido ?resposta))))

;; Regra para orientar o usuário a entrar em contato com o suporte
(defrule orientar-contato-suporte
   ?usuario <- (problema-usuario (nome-usuario ?nome) (tipo ?tipo))
   ?problema <- (problema (id ?tipo) (nome ?nome-problema))
   ?confirmacao <- (confirmacao-usuario (resolvido nao))
   =>
   (printout t crlf "Olá, " ?nome "!" crlf
              "O problema '" ?nome-problema "' ainda não foi resolvido." crlf
              "Por favor, entre em contato com o suporte pelo e-mail suporte@CyberTech.br" crlf
              "Informe os seguintes detalhes no e-mail:" crlf
              "- Seu nome: " ?nome crlf
              "- Problema: " ?nome-problema crlf
              "- ID do problema: " ?tipo crlf))

;; Regra para encerrar com uma mensagem positiva se o problema foi resolvido
(defrule problema-resolvido
   ?confirmacao <- (confirmacao-usuario (resolvido sim))
   =>
   (printout t crlf "Fico feliz que o problema foi resolvido!" crlf))


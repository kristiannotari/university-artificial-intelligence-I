# Macchina

Kristian Notari 892708

Relazione per il progetto di Intelligenza Artificiale I

## Obiettivo

Il progetto consiste nell'implementazione di un agente con conoscenza incompleta, che gestisca e pianifichi il ciclo di vita di una macchina da corsa all'interno di un circuito con avversari (altre macchine).

## Modalità e vincoli

La macchina ha un'usura (generica) massima che può "sopportare", mentre gli avversari no. Il suo scopo è completare n giri di un circuito, dalla griglia di partenza al traguardo. 

Vi è un circuito composto da una serie di sezioni (S), ognuna delle quali può avere n traiettorie (T). Il circuito è una lista ordinata di sezioni. E' poi caratterizzata la possibilità di effettuare pitstop, in modo da far tornare l'usura a 0. E' anche possibile tagliare il traguardo tornando ai box nel corso dell'ultimo giro. La sosta ai box ha un costo fisso determinato dal circuito più un costo proporzionale all'usura da "ripristinare".

Ad ogni spostamento di punto in punto (punto = coppia sezione-traiettoria), il circuito fornisce il costo in usura necessario a quello spostamento.

Anche gli avversari si spostano ma solo al termine di ogni giro, nella sezione successiva a quella dove si trovavano (per far si di non dover gestire diverse velocità per ogni macchina presente nel circuito ma permettere comunque scenari di sorpassi da effettuare). Non è possibile spostare la macchina in un punto occupato da un avversario (ne un avversario spostarsi in un punto occupato da un altro avversario).

## Organizzazione codice

Vi sono 3 file chiave:

- mondo_macchina.pl
- pianificatore_macchina.pl
- macchina.pl

### Mondo macchina

Il mondo macchina gestisce le regole, i vincoli e le azioni che sono vigenti o che possono essere effettuate all'interno di un mondo selezionato (caricato). Definisce anche l'usura massima della macchina. I mondi possibili si trovano nella cartella "mondi" e ve ne sono già presenti 4.

#### Mondi

Ogni mondo gestisce i parametri del circuito:

- composizione
- sezioni
- traiettorie
- numero di giri
- ingresso e uscita pitlane
- costo fisso del pitstop
- costo per gli spostamenti in un determinato punto
- numero e posizione degli avversari

Insieme al codice vi sono 4 mondi (su cui sono stati effettuati anche i test per le euristiche):

- mondo 1a (circuito del mugello, ridotto, 1 giro)
- mondo 1b (circuito del mugello, ridotto, 3 giri)
- mondo 2a (circuito del mugello, 1 giro)
- mondo 2b (circuito del mugello, 3 giri)

#### Predicati dinamici

I cambiamenti nel mondo, a seguito delle azioni possono essere:

- `in(luogo)` ovvero la posizione attuale della macchina (luogo = {box,punto(S,T)})
- `usura(number)` ovvero l'usura attuale della macchina
- `pitstop(number)` ovvero il numero di pitstop effettuati dalla macchina
- `giro(number)` ovvero il numero di giri già percorsi dalla macchina
- `avversario(atom,sezione,traiettoria)` ovvero l'enumerazione degli avversari (indicati con un atom, ad esempio "hamilton") e la loro posizione nel circuito

#### Azioni

La macchina può effettuare solo determinate azioni qui specificate:

- `schierati` che mette a default i predicati dinamici del mondo e muove la macchina dai box alla griglia di partenza
- `guida(p(sezione,traiettoria))` che muove la macchina dalla posizione attuale al punto specificato se possibile
- `effettua_pitstop` che permette alla macchina di fermarsi ai box per ripristinare 'usura, se attualmente la macchina si trova nel punto di pitlane_in (ovvero ingresso in pitlane) e che termina anche la gara tagliando il traguardo se dovesse essere l'ultimo giro
- `taglia_traguardo` che permette di tagliare il traguardo e terminare la corsa, riportando la macchina ai box

Le azioni come `guida` e `effettua_pitstop` contengono sottocasi in base alle reali condizioni della gara.

Lo spostamento degli avversari avviene ogni quando la macchina taglia il traguardo (completa un giro, che sia da circuito o dai box) con un ~10% di probabilità che gli avversari non scelgano il proprio punto successivo in modo ottimale (quello dal costo spostamento minore, a meno di occupazioni del punto da parte di un altro avversario o della macchina).

#### Animazioni

Ogni azione della macchina ha un'animazione associata (testuale) che dipende dal codice presente nel file animazione.pl.

#### Tests

In fondo al codice è presente una piccola utility fatta da alcuni predicati per poter testare una lista ordinata di azioni da far effettuare alla macchina, dato il mondo caricato.

### Pianificatore macchina

Il pianificatore macchina si occupa di una sola pianificazione: "gareggia". Questa si occupa di pianificare la lista di azioni migliore per far si che una macchina da una posizione arrivi al traguardo, rispettando i vincoli imposti dal mondo_macchina, completando la gara. Va in errore qualora non vi siano possibilità di completare la gara attraverso le azioni disponibili e i vincoli espressi.

#### Azioni pianificabili

L'implementazione del predicato add_del è chiusa a 3 casi:

- guida
- effettua_pitstop
- taglia_traguardo

che sono le 3 azioni possibili in fase di pianificazione, anche se internamente nascondono più sottocasi, in base alle pianificate condizioni della gara in quel dato momento.

Lo spostamento degli avversari viene pianificato essere quello (per loro) a minor costo di spostamento, sempre (a meno di occupazione del punto da parte di altri avversari).

#### Stati iniziale e finale

Quindi lo stato iniziale permette di "riprendere" la corsa da qualsiasi punto si voglia, non per forza dalla griglia di partenza, andando così a poter pianificare qualsiasi strategia che termini con lo stato finale.

Lo stato finale definisce le condizioni di fine gara che sono:

- macchina al box
- usura attuale macchina <= usura massima macchina
- i giri effettuati sono uguali ai giri richiesti dal circuito (mondo caricato)

e non serve altro poichè le azioni disponibili pianificabili attravero gli add_del fanno si che di stato in stato si rimanga all'interno di stati ammissibili (rispetto ai vincoli del mondo macchina e rispetto ai vincoli dello stato finale).

#### Euristiche

Vi sono presenti diverse euristiche (compresa quella nulla), testate nel file euristiche.pdf che permettono di visitare i nodi della pianificazione in modi differenti.

### Macchina

L'agente è stato implementato con conoscenza incompleta in quanto la pianificazione degli spostamenti degli avversari è indipendente dalla reale posizione di questi nel mondo macchina (in quanto c'è un discorso probabilistico legato allo spostamento degli avversari nel mondo macchina).

Nonostante questo non è stato necessario andare a modificare il codice di conoscenza_macchina.pl in quanto la pianificazione del pianificatore non tiene conto del fatto che sia una pianificazione in partenza della gara o una pianificazione a seguito di interruzioni nella stessa, dunque ogni volta che l'agente ritenta una pianificazione va automaticamente a ricredere vere le posizioni di partenza degli avversari controllando quelle reali del mondo macchina al momento dell'interruzione.

#### Stati

Gli stati possibili in cui può trovarsi l'agente macchina sono:

- ferma
- in_gara
- interruzione
- unknown (per i casi impossibili, non gestiti)

Le interruzioni possono essere:

- `punto_occupato_macchina` ovvero interruzione dovuta al fatto che la macchina non può muoversi nel punto pianificato in quanto si trova un avversario nella stessa posizione
- `punto_occupato_avversario` ovvero interruzione dovuta al fatto che un avversario non può muoversi nel punto scelto dal mondo macchina reale in quanto si trova un avversario o la macchina nella stessa posizione

#### Decisioni

Se è nello stato di `ferma` l'agente effettua la decisione semplice di schierarsi per la gara.

Se è nello stato di `in_gara` l'agente pianifica la decisione complessa "gareggia" che utilizza il pianificatore per creare una lista ordinata di azioni da eseguire.

Durante l'esecuzione delle azioni pianificate è possibile che capitino delle interruzioni. Se l'interruzione dovessere essere "sconosciuta" (unknown) l'agente abortisce la sua esecuzione.

Altrimenti è un'interruzione conosciuta che può essere gestita:

- `punto_occupato_macchina` la pianificazione viene rieseguita ottenendo come informazioni iniziali le posizioni reali degli avversari, in modo da ripianificare una serie di azioni per completare la gara
- `punto_occupato_avversario` si "simula" un'incidente per cui l'avversario che si sta muovendo in un punto occupato viene rimosso dalla gara in quanto incidentato

## Manuale d'uso

Per poter utilizzare l'agente è necessario caricare il file "macchina.pl" e avviarlo con il comando `start.`. A questo punto l'esecuzione apparirà a schermo e si potrà proseguire nei vari stati decisionali usando enter da tastiera.

Se si volesse cambiare il mondo caricato dal mondo_macchina è sufficiente andare nel file "mondo_macchina.pl" e, in fase di `clear_db` fare il `consult` del mondo scelto.

Si possono anche caricare gli altri file per testare a livello intermedio il pianificatore o il mondo macchina tenendo a mente che l'ordine gerarchico di caricamento degli stessi da parte del codice è:

macchina.pl > pianificatore_macchina.pl > mondo_macchina.pl > mondo\<X>.pl
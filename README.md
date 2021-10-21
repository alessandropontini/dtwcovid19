
# Progetto di tesi magistrale di Alessandro Pontini
## Usare il dynamic Time Warping per allineare delle serie temporali al tempo del Covid 19

# Indice

- [Introduzione](#Introduzione)
- [Come Installare ed Eseguire](#come-installare-ed-eseguire)
- [Ringraziamenti](#ringraziamenti)
- [Utilizzo](#utilizzo)

## Introduzione
Il 2021 è stato un anno che ha messo in discussione l'efficacia di molti modelli predittivi. Questo fenomeno è dovuto alla pandemia globale che si è scatenata a causa del virus Covid-19. Il mio progetto si colloca in una serie di patch per cui per ovviare ai problemi dovuti al covid-19 si è voluto utilizzare l'algoritmo del Dynamic Time Warping per allineare le curve di analisi. Questo script permette all'utente di eseguire l'allineamento ed al variare dei parametri come cambia questo. Inoltre sempre a livello di progetto è stato implementato un sistema di pesistica che alloca dinamicamente valore alle stagioni passate.

## Come installare
Per installarlo bisogna avere installato R e l'IDE R-studio. Una volta completata l'installazione è possibile scaricare la repository nel proprio computer. Il pacchetto creato per la simulazione si chiama *dtwweights* e può essere installato in due modi:

### Primo metodo consigliato
- Dentro la repository si trova una cartella con il formato tar.gz. contenente il pacchetto già pronto.
- Basta prenderela ed eseguire una installazione con il comando da R ```install.packages(path_to_file, repos = NULL, type="source") ```
- Più informazioni si possono trovare a questo link: https://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source

### Secondo metodo non consigliato
- E' possibile crearsi il proprio pacchetto direttamente da R-Studio
- Primo step è aprire da R-Studio il progetto già creato in dtw-weight.
- Secondo step è cliccare nel tab Build il comando Document.
- Terzo step è poi cliccare sempre nel tab Build il comando Install and Build. 
- Al quarto ed ultimo step è possibile uscire dal progetto e tornare nella cartella iniziale.

## Utilizzo
A questo punto siamo pronti per usare il pacchetto andando direttamente nel file R *main.R* ed eseguirlo nella sua totalità.

## Ringraziamenti
Ringrazio tutta Sdg per l'aiuto e per la disponibilità. 

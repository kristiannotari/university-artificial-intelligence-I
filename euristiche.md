# Euristiche

- Mondo 1a (mugello ridotto 1 giro)
- Mondo 1b (mugello ridotto 3 giri)
- Mondo 2a (mugello 1 giro)
- Mondo 2b (mugello 3 giri)
- Mondo 2c (mugello 10 giri)
- Mondo 2d (mugello 50 giri)

## Nessuna euristica

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 2c | Mondo 2d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 900      | 28706    | -        | -        | -        | -        |
| iterazioni            | 510      | 51578    | -        | -        | -        | -        |
| profondita' soluzione | 8        | 20       | -        | -        | -        | -        |
| costo                 | 7        | 34       | -        | -        | -        | -        |
| b                     | 2.3403   | 1.6707   | -        | -        | -        | -        |

## Euristica 1: distanza ancora da percorrere (in costo)

### Obiettivo

Calcolo la distanza (in costo) ancora da percorrere per arrivare al traguardo con la seguente formula: `numero di giri rimanenti * lunghezza giro * costo minimo traiettoria + costo minimo dalla sezione in cui mi trovo fino alla fine del giro corrente` dove la lunghezza del giro è data dal conteggio delle sezioni presenti nel circuito ed entrambi gli addendi sono pesati con dei pesi W1 e W2.

L'euristica è sottostimata (scelgo sempre quantità di tempo minima per passare dalla sezione attuale al primo traguardo disponibile nel secondo addendo di cui qui sopra) e come si può vedere nel confronto tra le due seguenti tabelle, aumentare l'impatto dell'euristica nella raimificazione con pesi W1=4 e W2=4 (che sono stati scelti con tali valori perchè portano alle prestazioni di calcolo migliori) comportano un risultato non ottimale per il mondo 1b, che ha la peculiarità di essere un mondo con 3 giri in un circuito breve.

### Risultati pesi 1, 1

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 2c | Mondo 2d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 44       | 7306     | 9864     | 408933   | -        | -        |
| iterazioni            | 16       | 9334     | 10118    | 709206   | -        | -        |
| profondita' soluzione | 8        | 20       | 23       | 63       | -        | -        |
| costo                 | 7        | 34       | 33       | 178      | -        | -        |
| b                     | 1.6048   | 1.5602   | 1.4916   | 1.2276   | -        | -        |

### Risultati pesi 4, 4

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 2c | Mondo 2d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 164      | 254      | 159      | 1291     | 5230     | 25630    |
| iterazioni            | 206      | 334      | 160      | 2190     | 9088     | 44848    |
| profondita' soluzione | 8        | 19       | 22       | 64       | 207      | 1027     |
| costo                 | 7        | 45       | 45       | 182      | 688      | 3668     |
| b                     | 1.8917   | 1.3383   | 1.2591   | 1.1184   | 1.0422   | 1.0099   |

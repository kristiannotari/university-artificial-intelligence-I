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
| nodi espansi          | 924      | 51721    | -        | -        | -        | -        |
| iterazioni            | 414      | 95820    | -        | -        | -        | -        |
| profondita' soluzione | 8        | 20       | -        | -        | -        | -        |
| costo                 | 7        | 34       | -        | -        | -        | -        |
| b                     | 2.3480   | 1.7206   | -        | -        | -        | -        |

## Euristica 1: distanza ancora da percorrere (in costo)

### Obiettivo

Calcolo la distanza (in costo) ancora da percorrere per arrivare al traguardo con la seguente formula: `numero di giri rimanenti * lunghezza giro * costo minimo traiettoria + costo minimo dalla sezione in cui mi trovo fino alla fine del giro corrente` dove la lunghezza del giro è data dal conteggio delle sezioni presenti nel circuito.

### Risultati pesi 1, 1

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 2c | Mondo 2d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 44       | 13971    | 540182   | -        | -        | -        |
| iterazioni            | 16       | 19624    | 338608   | -        | -        | -        |
| profondita' soluzione | 8        | 20       | 23       | -        | -        | -        |
| costo                 | 7        | 34       | 42       | -        | -        | -        |
| b                     | 1.6048   | 1.6116   | 1.7751   | -        | -        | -        |

### Risultati pesi 4, 4

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 2c | Mondo 2d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 164      | 199      | 146      | 1345     | 5537     | 27157    |
| iterazioni            | 206      | 240      | 136      | 2302     | 9712     | 47952    |
| profondita' soluzione | 8        | 19       | 22       | 64       | 207      | 1027     |
| costo                 | 7        | 45       | 45       | 182      | 688      | 3668     |
| b                     | 1.8917   | 1.3212   | 1.2542   | 1.1191   | 1.0425   | 1.0099   |

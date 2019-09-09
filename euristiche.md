# Euristiche

Usura massima: 25

*Usura sufficiente per 1 giro del circuito non ridotto ma non per 3 giri (necessario pitstop)*

- Mondo 1a (mugello ridotto 1 giro)
- Mondo 1b (mugello ridotto 3 giri)
- Mondo 2a (mugello 1 giro)
- Mondo 2b (mugello 3 giri)
- Mondo 3a (mugello ridotto 1 giro 2 avversari)
- Mondo 3b (mugello ridotto 3 giri 2 avversari)
- Mondo 3c (mugello 1 giro 2 avversari)
- Mondo 3d (mugello 3 giri 2 avversari)

## Nessuna euristica

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 3a | Mondo 3b | Mondo 3c | Mondo 3d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 19       | -        | 272539   | -        | 311      | -        | 288026   | -        |
| iterazioni            | 34       | -        | 243086   | -        | 326      | -        | 257920   | -        |
| profondita' soluzione | 10       | -        | 23       | -        | 10       | -        | 23       | -        |
| costo                 | 8        | -        | 21       | -        | 8        | -        | 21       | -        |
| b                     | 1.3423   | -        | 1.7231   | -        | 1.7753   | -        | 1.7272   | -        |

## Euristica 1: distanza ancora da percorrere (in costo)

### Obiettivo

Calcolo la distanza (in costo) ancora da percorrere per arrivare al traguardo con la seguente formula: `numero di giri rimanenti * lunghezza giro * costo minimo traiettoria + costo traiettoria attuale` dove la lunghezza del giro è data dal conteggio delle sezioni presenti nel circuito.

### Risultati

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a | Mondo 2b | Mondo 3a | Mondo 3b | Mondo 3c | Mondo 3d |
| --------------------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- | -------- |
| nodi espansi          | 19       | 7745     | 3841     | 15106    | 203      | 7372     | 4940     | 18224    |
| iterazioni            | 34       | 10148    | 5020     | 27936    | 236      | 13334    | 6314     | 34526    |
| profondita' soluzione | 10       | 26       | 23       | 63       | 10       | 25       | 23       | 63       |
| costo                 | 8        | 24       | 21       | 112      | 8        | 38       | 21       | 115      |
| b                     | 1.3423   | 1.4111   | 1.4316   | 1.1650   | 1.7011   | 1.4279   | 1.4474   | 1.1685   |

# Euristiche

- Mondo 1a (mugello ridotto 1 giro)
- Mondo 1b (mugello ridotto 3 giri)
- Mondo 2a (mugello 1 giro)

## Nessuna euristica

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a |
| --------------------- | -------- | -------- | -------- |
| nodi espansi          | 924      | 51721    | -        |
| iterazioni            | 414      | 95820    | -        |
| profondita' soluzione | 8        | 20       | -        |
| costo                 | 7        | 34       | -        |
| b                     | 2.3480   | 1.7206   | -        |

## Euristica 1: distanza ancora da percorrere (in costo)

### Obiettivo

Calcolo la distanza (in costo) ancora da percorrere per arrivare al traguardo con la seguente formula: `numero di giri rimanenti * lunghezza giro * costo minimo traiettoria + costo traiettoria attuale (velocità migliore)` dove la lunghezza del giro è data dal conteggio delle sezioni presenti nel circuito.

### Risultati

| Proprietà             | Mondo 1a | Mondo 1b | Mondo 2a |
| --------------------- | -------- | -------- | -------- |
| nodi espansi          | 44       | 13971    | 540182   |
| iterazioni            | 16       | 19624    | 338608   |
| profondita' soluzione | 8        | 20       | 23       |
| costo                 | 7        | 34       | 42       |
| b                     | 1.6048   | 1.6116   | 1.7751   |

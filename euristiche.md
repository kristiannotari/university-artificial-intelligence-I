# Euristiche

Usura massima: 40
> sufficiente per un giro singolo senza passaggio dai box (condizioni ottimali) ma necessario box per più giri

- Mondo 1a (mugello ridotto 1 giro)
- Mondo 1b (mugello ridotto 3 giri)
- Mondo 2a (mugello 1 giro)
- Mondo 2b (mugello 3 giri)

## Nessuna euristica

| Proprietà             | Mondo 1a           | Mondo 1b | Mondo 2a | Mondo 2b |
| --------------------- | ------------------ | -------- | -------- | -------- |
| nodi espansi          | 22                | -        | -        | -        |
| iterazioni            | 36                | -        | -        | -        |
| profondita' soluzione | 10                 | -        | -        | -        |
| costo                 | 8                  | -        | -        | -        |
| b                     | 1.362204366553743 | -        | -        | -        |

## Euristica 1: distanza ancora da percorrere (in costo)

### Obiettivo

Calcolo la distanza (in costo) ancora da percorrere per arrivare al traguardo con la seguente formula: `numero di giri rimanenti * lunghezza giro * costo minimo traiettoria + costo traiettoria attuale` dove la lunghezza del giro è data dal conteggio delle sezioni presenti nel circuito.

### Risultati

| Proprietà             | Mondo 1a           | Mondo 1b           | Mondo 2a           | Mondo 2b |
| --------------------- | ------------------ | ------------------ | ------------------ | -------- |
| nodi espansi          | 22                | 32713              | 5594               | -        |
| iterazioni            | 36                | 28564              | 4928               | -        |
| profondita' soluzione | 10                 | 26                 | 23                 | -        |
| costo                 | 8                  | 26                 | 21                 | -        |
| b                     | 1.362204366553743 | 1.4915681161867618 | 1.4552729430739644 | -        |

## Euristica 2: enfasi costo traiettoria e costo traiettoria avversari

### Obiettivo

Come euristica 1 ma enfatizzo anche il costo di traiettoria degli avversari, preferendo ramificazioni dove gli avversari abbiano scelto una loro traiettoria a costo ridotto.

### Risultati

| Proprietà             | Mondo 1a          | Mondo 1b          | Mondo 2a           | Mondo 2b |
| --------------------- | ----------------- | ----------------- | ------------------ | -------- |
| nodi espansi          | 79                | 10168             | 2758               | -        |
| iterazioni            | 48                | 8816              | 2334               | -        |
| profondita' soluzione | 10                | 26                | 23                 | -        |
| costo                 | 9                 | 26                | 22                 | -        |
| b                     | 1.547970603881666 | 1.426016149051916 | 1.4112080815442836 | -        |

## Euristica 3: eliminazione vincolo costo traiettoria

### Obiettivo

Calcolo il costo come il costo traiettoria negato, eliminando di fatto il costo dato dalla pianificazione di questa traiettoria, rilassando il vincolo.

### Risultati

| Proprietà             | Mondo 1a          | Mondo 1b | Mondo 2a | Mondo 2b |
| --------------------- | ----------------- | -------- | -------- | -------- |
| nodi espansi          | 2972              | -        | -        | -        |
| iterazioni            | 1678              | -        | -        | -        |
| profondita' soluzione | 10                | -        | -        | -        |
| costo                 | 8                 | -        | -        | -        |
| b                     | 2.224871231774643 | -        | -        | -        |

## Euristica 4: eliminazione vincolo costo traiettoria e costo pitstop

### Obiettivo

Calcolo il costo come euristica 3 o, nel caso di pitstop tra lo stato precedente e questo, come costo del pitstop negato.

### Risultati

| Proprietà             | Mondo 1a          | Mondo 1b | Mondo 2a | Mondo 2b |
| --------------------- | ----------------- | -------- | -------- | -------- |
| nodi espansi          | 2972              | -        | -        | -        |
| iterazioni            | 1678              | -        | -        | -        |
| profondita' soluzione | 10                | -        | -        | -        |
| costo                 | 8                 | -        | -        | -        |
| b                     | 2.224871231774643 | -        | -        | -        |

This is my C version of the Chess AI

I haven't used C too much but I'm learing a lot really quickly

Pointers have not been as tricky as I thought they'd be

Currently working on Zobrist Hashing in C (It works in Java but that is from school) It allows you to store visited positions and their evaluations, allowing you to explore other parts of the game tree.
There is a balance between hashing speed and converting between the hashable input and useable board state. The pieces are stored in uint64_t numbers where a 1 bit represents the presents of a piece. Using a DeBruin Sequence
allows for quick access to the index positions of a 1 bit. These bitboards allow you to generate moves very quickly, Rooks, Bishops, and Queens all can you hyperbole quintessence to generate their moves(some simple bit 
math/manipulation creates the movement for sliding pieces for a given row) Kings and Knights can use a simple table look up, while pawns use bit masks. 

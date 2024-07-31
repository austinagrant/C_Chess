#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX(x, y) ((x) > (y) ? (x) : (y))

// indexing into castle rights
typedef enum {
    King_W,
    Queen_W,
    King_B,
    Queen_B,
    None,
} Castle;

typedef enum  {
    White_King,
    White_Queen,
    White_Rook,
    White_Bishop,
    White_Knight,
    White_Pawn,
    Black_King,
    Black_Queen,
    Black_Rook,
    Black_Bishop,
    Black_Knight,
    Black_Pawn,
    BLANK
}Piece;


typedef struct
{
    Piece piece_type;
    int start_index;
    uint64_t moves;
    Piece taken;
    Castle castle_side;
    Piece transform;
    int eval;
} Move;
uint8_t anti_diag_mak[] = {
        0b11111111,
        0b1, 0b11111110,
        0b11, 0b11111100,
        0b111, 0b11111000,
        0b1111, 0b11110000,
        0b11111, 0b11100000,
        0b111111, 0b11000000,
        0b1111111, 0b10000000
};


uint8_t diag_mask[] = {
        0b11111111,
        0b1111111, 0b10000000,
        0b111111, 0b11000000,
        0b11111, 0b11100000,
        0b1111, 0b11110000,
        0b111, 0b11111000,
        0b11, 0b11111100,
        0b1, 0b11111110
};

int calc_index(int row, int col){
        if (row == 0)
            return 0;
        else if (col < (8 - row))
            return 2 * row - 1;
        else
            return 2 * row;
}

int calc_anti_diag_index(int row, int col){
        if (row == 0)
            return 0;
        else if (col < row)
            return 2 * row - 1;
        else
            return 2 * row;
}




int debruin[64] = {63, 0, 58, 1, 59, 47, 53, 2, 60, 39,
                48, 27, 54, 33, 42, 3, 61, 51, 37, 40, 49, 18, 28,
                20, 55, 30, 34, 11, 43, 14, 22, 4, 62, 57, 46, 52,
                38, 26, 32, 41, 50, 36, 17, 19, 29, 10, 13, 21, 56,
                45, 25, 31, 35, 16, 9, 12, 44, 24, 15, 8, 23, 7, 6, 5};
// '♔',  '♕,' ,    '♖',      '♗',     '♘',      '♙',   '♚',     '♛',      '♜',     '♝',     '♞',     '♟︎'

void printUInt64AsBinary(uint64_t value) {
    const int bits = 64; // 64 bits for uint64_t
    char binary[65]; // +1 for the null terminator

    // Fill the binary array with bits in reverse order
    for (int i = 0; i < bits; i++) {
        binary[i] = (value & (1ULL << (bits - 1 - i))) ? '1' : '0';
    }
    binary[bits] = '\0'; // Null-terminate the string

    // Print the binary string with newline every 8 characters
    for (int i = 0; i < bits; i += 8) {
        for (int j = 7; j >= 0; j--) {
            putchar(binary[i + j]);
        }
        putchar('\n'); // Print a newline after each 8-bit segment
    }
    putchar('\n'); // Ensure a newline at the end
}

int get_MSB(uint64_t board){
        uint64_t deB = 0x07EDD5E59A4E28C2ULL;
        uint64_t deb_index = (((board & -board) * deB) >> 58) & 0x3f;
        return debruin[deb_index];
    }

uint64_t flip_vertical(uint64_t board){
        uint64_t k1 = 0x00FF00FF00FF00FF;
        uint64_t k2 = 0x0000FFFF0000FFFF;
        board = ((board >>  8) & k1) | ((board & k1) <<  8);
        board = ((board >> 16) & k2) | ((board & k2) << 16);
        board = ( board >> 32)       | ( board       << 32);
        return board;
}

uint64_t flipDiagA1H8(uint64_t board){
        uint64_t k1 = 0x5500550055005500;
        uint64_t k2 = 0x3333000033330000;
        uint64_t k4 = 0x0f0f0f0f00000000;
        uint64_t t;
        t      =  k4 & (board ^ (board << 28));
        board ^=  t ^ (t >> 28);
        t      =  k2 & (board ^ (board << 14));
        board ^=  t ^ (t >> 14);
        t      =  k1 & (board ^ (board <<  7));
        board ^=  t ^ (t >>  7);
        
        return board & 0xffffffffffffffff;
}

uint64_t flipDiagH1A8(uint64_t board){
        uint64_t k1 = 0xaa00aa00aa00aa00;
        uint64_t k2 = 0xcccc0000cccc0000;
        uint64_t k4 = 0xf0f0f0f00f0f0f0f;
        uint64_t t;
        t      =  board ^ (board << 36); 
        board ^=  k4 & (t ^ (board >> 36));
        t      =  k2 & (board ^ (board << 18));
        board ^=  t ^ (t >> 18); 
        t      =  k1 & (board ^ (board <<  9));
        board ^=  t ^ (t >>  9);
        return board & 0xffffffffffffffff;
}

uint64_t rotate90Clock(uint64_t board){
        return flipDiagH1A8(flip_vertical(board));
}

uint64_t rotate90Counter(uint64_t board){
        return flipDiagA1H8(flip_vertical(board));
}

uint64_t right_rotate(uint64_t to_shift, int shifted_by){
        return ((to_shift >> shifted_by) | (to_shift << (64 - shifted_by)));
}

uint64_t pseudoRotate45Clock(uint64_t board){
        uint64_t k1 = 0xAAAAAAAAAAAAAAAA;
        uint64_t k2 = 0xCCCCCCCCCCCCCCCC;
        uint64_t k4 = 0xF0F0F0F0F0F0F0F0;
        board ^= k1 & (board ^ right_rotate(board, 8));
        board ^= k2 & (board ^ right_rotate(board, 16));
        board ^= k4 & (board ^ right_rotate(board, 32));
        return board;
}

uint64_t pseudoRotate45Counter(uint64_t board){
        uint64_t k1 = 0x5555555555555555;
        uint64_t k2 = 0x3333333333333333;
        uint64_t k4 = 0x0f0f0f0f0f0f0f0f;
        board ^= k1 & (board ^ right_rotate(board,  8));
        board ^= k2 & (board ^ right_rotate(board, 16));
        board ^= k4 & (board ^ right_rotate(board, 32));
        return board;
}

typedef struct 
{
    uint64_t *en_passant;
    int en_passant_counter;
    uint64_t pieces[12];
    uint64_t white_team_board;
    uint64_t black_team_board;
    char castle[4];
    Piece mailbox[64];
} Board;

Piece get_Piece_at(Board *b, int pos){
    return b->mailbox[pos];
}

static const uint8_t reverse_byte_table[] = {
        0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
        0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
        0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
        0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
        0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
        0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
        0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
        0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
        0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
        0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
        0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
        0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
        0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
        0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
        0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
        0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
        0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
        0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
        0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
        0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
        0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
        0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
        0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
        0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
        0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
        0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
        0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
        0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
        0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
        0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
        0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
        0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff,
    };

uint8_t reverse_byte(uint8_t x){
    return reverse_byte_table[x];
}

wchar_t* pop_board(Board *b){
    wchar_t* board = (wchar_t*)calloc(64, sizeof(wchar_t));
    for(int i =0;  i < 64; i++){
        board[i] = L' ';
    }
    if (board == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    wchar_t piece_chars[] = {L'♔', L'♕', L'♖', L'♗', L'♘', L'♙', L'♚', L'♛', L'♜', L'♝', L'♞', 0x265F, L' '};
    for(int i = 0; i < 12; i++){
        uint64_t bitboard = b->pieces[i];
        while (bitboard != 0) {
            int index = get_MSB(bitboard);
            board[index] = piece_chars[i];
            b->mailbox[index] = i;
            bitboard = bitboard ^ (1ULL << index);
        }
    }
    return board;
}

void print_board(Board *b){
    wchar_t *board = pop_board(b);
    wchar_t string[989] = {L"+----------------------------------------+\n|                                        |\n|     +---+---+---+---+---+---+---+---+  |\n|  8  | "};
    for(int i = 63; i > 0; i-=8){
        for(int j=7; j > -1; j--){
            wchar_t elem[10];
            swprintf(elem, sizeof(elem)/sizeof(wchar_t), L"%lc", board[i - j]);
            wcscat(string, elem);
            wcscat(string, L" | ");
            if(j== 0 && i != 7){
                wchar_t line[] = {((i - j) / 8) + '0', '\0'};
                wcscat(string, L" |\n|     +---+---+---+---+---+---+---+---+  |\n|  ");
                wcscat(string, line);
                wcscat(string, L"  | ");
            }
        }
    }
    wcscat(string, L" |\n|     +---+---+---+---+---+---+---+---+  |\n|                                        |\n|       A   B   C   D   E   F   G   H    |\n|                                        |\n+----------------------------------------+\n");
    wprintf(L"%ls\n", string);
}

Move* generateRookMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_Rook] : b->pieces[Black_Rook];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    int piece_count = __builtin_popcountll(board);
    Move* moves = (Move*)calloc(MAX(piece_count, 1) , sizeof(Move));
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if (piece_count == 0){
        moves[0].start_index = 65;
        moves[0].piece_type = white ? White_Rook : Black_Rook;
        return moves;
    }
    for(int i = 0; i < piece_count; i++){
        int pos = get_MSB(board);
        board = board ^ (1ULL << pos);
        int row = pos / 8;
        uint8_t shift_row = 0b11111111;
        uint8_t rook = (1 << (pos % 8));
        uint8_t team_row = ((team_board >> (row * 8)) & shift_row);
        uint8_t opp_row = ((opp_board >> (row * 8)) & shift_row);
        
        uint8_t fin = ((team_row - (rook << 1)) ^ team_row) & ~team_row;
        
        uint8_t fin2 = (opp_row ^ (opp_row - (rook << 1)));
        uint8_t reverse_rook = reverse_byte(rook);
        uint8_t reverse_team_row = reverse_byte(team_row);
        uint8_t reverse_opp_row = reverse_byte(opp_row);
        
        team_row = reverse_team_row & shift_row;
        opp_row = reverse_opp_row & shift_row;
        uint8_t fin3 = reverse_byte((((team_row - (reverse_rook << 1)) ^ team_row) & ~team_row) & shift_row);
        uint8_t fin4 = reverse_byte((opp_row ^ (opp_row - (reverse_rook << 1))) & shift_row);
        uint8_t move_row = ((fin | fin3) & (fin2 | fin4)) & shift_row;
        uint64_t move_board = (uint64_t)move_row << (row * 8);

        // columns
        uint64_t  rotated_rook = rotate90Clock(1ULL << pos);
        int pos_rot = get_MSB(rotated_rook);
        uint64_t team_board_rot = rotate90Clock(team_board);
        uint64_t opp_board_rot = rotate90Clock(opp_board);
        row = pos_rot / 8;
        rook = 1 << (pos_rot % 8);
        team_row = (team_board_rot >> (row * 8)) & shift_row;
        opp_row = (opp_board_rot >> (row * 8)) & shift_row;
        
        fin = (((team_row - (rook << 1)) ^ team_row) & ~team_row ) & shift_row;
        fin2 = (opp_row ^ (opp_row - (rook << 1))) & shift_row;
        reverse_rook = reverse_byte(rook);
        reverse_team_row = reverse_byte(team_row);
        reverse_opp_row = reverse_byte(opp_row);
        
        team_row = reverse_team_row & shift_row;
        opp_row = reverse_opp_row & shift_row;
        
        fin3 = reverse_byte((((team_row - (reverse_rook << 1)) ^ team_row) & ~team_row) & shift_row);
        fin4 = reverse_byte((opp_row ^ (opp_row - (reverse_rook << 1))) & shift_row);
        move_row = ((fin | fin3) & (fin2 | fin4)) & shift_row;
        move_board |= rotate90Counter((uint64_t)move_row << (row * 8));
        moves[i].piece_type = white ? White_Rook : Black_Rook;
        moves[i].start_index = pos;
        moves[i].moves = move_board;
    }
    return moves;
}

Move* generateBishopMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_Bishop] : b->pieces[Black_Bishop];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    int piece_count = __builtin_popcountll(board);
    Move* moves = (Move*)calloc(MAX(piece_count,1) , sizeof(Move));
    
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if (piece_count == 0){
        moves[0].start_index = 65;
        moves[0].piece_type = white ? White_Bishop : Black_Bishop;
        return moves;
    }
    for(int i = 0; i < piece_count; i++){
        int pos = get_MSB(board);
        board = board ^ (1ULL << pos);
        uint64_t rot_pos = pseudoRotate45Clock((1ULL << pos));
        int rot_ind = get_MSB(rot_pos);
        uint64_t team_board_45 = pseudoRotate45Clock(team_board);
        uint64_t opp_board_45 = pseudoRotate45Clock(opp_board);
        int row = rot_ind / 8;
        int col = rot_ind % 8;
        uint8_t shift_row = 0b11111111;
        uint8_t bishop = (uint8_t)1 << col;
        uint8_t team_row = (uint8_t)((team_board_45 >> (row * 8)) & shift_row);
        uint8_t opp_row = (uint8_t)((opp_board_45 >> (row * 8)) & shift_row);
        
        uint8_t fin = ((team_row - (bishop << 1)) ^ team_row) & ~team_row;
        
        uint8_t fin2 = (opp_row ^ (opp_row - (bishop << 1)));
        
        uint8_t reverse_bishop = reverse_byte(bishop);
        team_row = reverse_byte(team_row);
        opp_row = reverse_byte(opp_row);
        
        uint8_t fin3 = reverse_byte((((team_row - (reverse_bishop << 1)) ^ team_row) & ~team_row) & shift_row);
        uint8_t fin4 = reverse_byte((opp_row ^ (opp_row - (reverse_bishop << 1))) & shift_row);
        uint8_t move_row = ((fin | fin3) & (fin2 | fin4));
        move_row &= diag_mask[calc_index(row, col)];
        uint64_t rot_board = ((uint64_t)move_row << (row * 8));
        uint64_t unrot_board = pseudoRotate45Counter(rot_board);
        uint64_t move_board = right_rotate(unrot_board, 8);

        // Antidiag
        uint64_t  rotated_bishop = pseudoRotate45Counter((uint64_t)1 << pos);
        rot_ind = get_MSB(rotated_bishop);
        team_board_45 = pseudoRotate45Counter(team_board);
        opp_board_45 = pseudoRotate45Counter(opp_board);
        row = rot_ind / 8;
        col = rot_ind % 8;
        bishop = 1 << col;
        team_row = (team_board_45 >> (row * 8)) & shift_row;
        opp_row = (opp_board_45 >> (row * 8)) & shift_row;
        
        fin = (((team_row - (bishop << 1)) ^ team_row) & ~team_row ) & shift_row;
        fin2 = (opp_row ^ (opp_row - (bishop << 1))) & shift_row;

        reverse_bishop = reverse_byte(bishop);
        team_row = reverse_byte(team_row);
        opp_row = reverse_byte(opp_row);
        
        // team_row = reverse_team_row & shift_row;
        // opp_row = reverse_opp_row & shift_row;
        
        fin3 = reverse_byte((((team_row - (reverse_bishop << 1)) ^ team_row) & ~team_row) & shift_row);
        fin4 = reverse_byte((opp_row ^ (opp_row - (reverse_bishop << 1))) & shift_row);
        move_row = ((fin | fin3) & (fin2 | fin4)) & shift_row;
        move_row &= anti_diag_mak[calc_anti_diag_index(row, col)];
        move_board |= right_rotate(pseudoRotate45Clock((uint64_t)move_row << (row * 8)), 8);
        moves[i].piece_type = white ? White_Bishop : Black_Bishop;
        moves[i].start_index = pos;
        moves[i].moves = move_board;
    }
    return moves;
}

Move* generateQueenMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_Queen] : b->pieces[Black_Queen];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    int piece_count = __builtin_popcountll(board);
    Move* moves = (Move*)calloc(MAX(piece_count, 1) , sizeof(Move));
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if (piece_count == 0){
        moves[0].start_index = 65;
        moves[0].piece_type = white ? White_Queen : Black_Queen;
        return moves;
    }
    Move* rook_moves = generateRookMoves(&((Board){.pieces[White_Rook] = b->pieces[White_Queen], .pieces[Black_Rook] = b->pieces[Black_Queen], .white_team_board = b->white_team_board, .black_team_board = b->black_team_board}), white);
    Move* bishop_moves = generateBishopMoves(&((Board){.pieces[White_Bishop] = b->pieces[White_Queen], .pieces[Black_Bishop] = b->pieces[Black_Queen], .white_team_board = b->white_team_board, .black_team_board = b->black_team_board}), white);
    for(int i = 0; i < piece_count; i++){
        uint64_t rook_board = rook_moves[i].moves;
        uint64_t bishop_board = bishop_moves[i].moves;
        moves[i].start_index = rook_moves->start_index;
        moves[i].piece_type = white ? White_Queen : Black_Queen;
        moves[i].moves = bishop_board;
    }
    return moves;
}

typedef enum {
    Alt_1,
    Alt_2,
    Attack_1,
    Attack_2,
    En_Passant_1,
    En_Passant_2,
    Num_Pawn_Moves
} Pawn_Move;

int calc_pawn_start(Pawn_Move type, bool white, int end_index){
    int start_index = 65;
    if (white){
        switch (type){
            case Attack_1:
                start_index = end_index-9;
                break;
            case Attack_2:
                start_index = end_index-7;
                break;
            case Alt_1:
                start_index = end_index-8;
                break;
            case Alt_2:
                start_index = end_index-16;
                break;
            case En_Passant_1:
                start_index = end_index-9;
                break;
            case En_Passant_2:
                start_index = end_index-7;
                break;
        }
    }
    else {
        switch (type){
            case Attack_1:
                start_index = end_index+7;
                break;
            case Attack_2:
                start_index = end_index+9;
                break;
            case Alt_1:
                start_index = end_index+8;
                break;
            case Alt_2:
                start_index = end_index+16;
                break;
            case En_Passant_1:
                start_index = end_index+7;
                break;
            case En_Passant_2:
                start_index = end_index+9;
                break;
        }
    }
    return start_index;
}



Move* generatePawnMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_Pawn] : _byteswap_uint64(b->pieces[Black_Pawn]);
    uint64_t team_board = white ? b->white_team_board : _byteswap_uint64(b->black_team_board);
    uint64_t opp_board = white ? b->black_team_board : _byteswap_uint64(b->white_team_board);
    uint64_t en_passant = white ? b->en_passant[b->en_passant_counter] : _byteswap_uint64(b->en_passant[b->en_passant_counter]);
    int piece_count = __builtin_popcountll(board);
    Move* moves = (Move*)calloc(6 , sizeof(Move));
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if (piece_count == 0){
        moves[0].start_index = 65;
        moves[0].start_index = white ? White_Pawn : Black_Pawn;
        return moves;
    }
    uint64_t occ = team_board | opp_board;
    uint64_t alt_move_rule_1 = (board & ~(occ >> 8)) << 8;
    uint64_t alt_move_rule_2 = (0xff00 & (alt_move_rule_1 >> 8) & ~(occ >> 16)) << 16;
    uint64_t alt_attack_rule_1 = (board & 0x7f7f7f7f7f7f7f7f & (opp_board >> 9)) << 9;
    uint64_t alt_attack_rule_2 = (board & 0xfefefefefefefefe & (opp_board >> 7)) << 7;
    uint64_t en_passant_1 = (board & 0x7f7f7f7f7f7f7f7f & (en_passant >> 9)) << 9;
    uint64_t en_passant_2 = (board & 0xfefefefefefefefe & (en_passant >> 7)) << 7;
    if (!white){
        board = _byteswap_uint64(board);
        alt_move_rule_1 = _byteswap_uint64(alt_move_rule_1);
        alt_move_rule_2 = _byteswap_uint64(alt_move_rule_2);
        alt_attack_rule_1 = _byteswap_uint64(alt_attack_rule_1);
        alt_attack_rule_2 = _byteswap_uint64(alt_attack_rule_2);
        en_passant_1 = _byteswap_uint64(en_passant_1);
        en_passant_2 = _byteswap_uint64(en_passant_2);
    }
    int piece_type = white ? White_Pawn : Black_Pawn;
    for(int i = 0; i < Num_Pawn_Moves; i++){
        moves[i].start_index = i;
    }
    moves[Alt_1].moves = alt_move_rule_1;
    moves[Alt_2].moves = alt_move_rule_2;
    moves[Attack_1].moves = alt_attack_rule_1;
    moves[Attack_2].moves = alt_attack_rule_2;
    moves[En_Passant_1].moves = en_passant_1;
    moves[En_Passant_2].moves = en_passant_2;
    moves[Alt_1].piece_type = piece_type;
    moves[Alt_2].piece_type = piece_type;
    moves[Attack_1].piece_type = piece_type;
    moves[Attack_2].piece_type = piece_type;
    moves[En_Passant_1].piece_type = piece_type;
    moves[En_Passant_2].piece_type = piece_type;
    return moves;
}

uint64_t knight_move_table[] = {
    132096, 329728, 659712, 1319424, 2638848, 5277696, 10489856, 4202496, 33816580, 84410376, 168886289, 337772578, 675545156, 1351090312, 2685403152, 1075839008, 8657044482, 21609056261, 43234889994, 86469779988, 
    172939559976, 345879119952, 687463207072, 275414786112, 2216203387392, 5531918402816, 11068131838464, 22136263676928, 44272527353856, 88545054707712, 175990581010432, 70506185244672, 567348067172352, 1416171111120896, 
    2833441750646784, 5666883501293568, 11333767002587136, 22667534005174272, 45053588738670592, 18049583422636032, 145241105196122112, 362539804446949376, 725361088165576704, 1450722176331153408, 2901444352662306816, 5802888705324613632, 
    11533718717099671552, 4620693356194824192, 288234782788157440, 576469569871282176, 1224997833292120064, 2449995666584240128, 4899991333168480256, 9799982666336960512, 1152939783987658752, 2305878468463689728, 1128098930098176, 
    2257297371824128, 4796069720358912, 9592139440717824, 19184278881435648, 38368557762871296, 4679521487814656, 9077567998918656};

Move* generateKnightMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_Knight] : b->pieces[Black_Knight];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    int piece_count = __builtin_popcountll(board);
    Move* moves = (Move*)calloc(MAX(piece_count, 1) , sizeof(Move));
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if (piece_count == 0){
        moves[0].start_index = 65;
        moves[0].piece_type = white ? White_Knight : Black_Knight;
        return moves;
    }
    for(int i = 0; i < piece_count; i++){
        int pos = get_MSB(board);
        board = board ^ (1ULL << pos);
        uint64_t move_board = knight_move_table[pos];
        move_board = (move_board ^ team_board) & move_board;
        moves[i].start_index = pos;
        moves[i].piece_type = white ? White_Knight : Black_Knight;
        moves[i].moves = move_board;
    }
    return moves;
}

uint64_t king_move_table[] = {
    770, 1797, 3594, 7188, 14376, 28752, 57504, 49216, 197123, 460039, 920078, 1840156, 3680312, 7360624, 14721248, 
    12599488, 50463488, 117769984, 235539968, 471079936, 942159872, 1884319744, 3768639488, 3225468928, 12918652928, 30149115904, 
    60298231808, 120596463616, 241192927232, 482385854464, 964771708928, 825720045568, 3307175149568, 7718173671424, 15436347342848, 
    30872694685696, 61745389371392, 123490778742784, 246981557485568, 211384331665408, 846636838289408, 1975852459884544, 3951704919769088, 
    7903409839538176, 15806819679076352, 31613639358152704, 63227278716305408, 54114388906344448, 216739030602088448, 505818229730443264, 
    1011636459460886528, 2023272918921773056, 4046545837843546112, 8093091675687092224, 16186183351374184448, 13853283560024178688, 
    144959613005987840, 362258295026614272, 724516590053228544, 1449033180106457088, 2898066360212914176, 5796132720425828352, 11592265440851656704, 4665729213955833856
};

uint64_t valid_checks(Board *b, bool white, uint64_t king_moves){
    // Pawn Checks
    uint64_t pawn_1 = king_moves & 0x7f7f7f7f7f7f7f7f & ((white ? b->pieces[Black_Pawn] : b->pieces[White_Pawn]) >> 9);
    uint64_t pawn_2 = king_moves & 0xfefefefefefefefe & ((white ? b->pieces[Black_Pawn] : b->pieces[White_Pawn]) >> 7);
    king_moves = (king_moves ^ (pawn_1 | pawn_2)) & king_moves;
    uint64_t king_moves_copy = king_moves;
    while (king_moves){
        int pos = (get_MSB(king_moves));
        uint64_t curr_move = (1ULL << (get_MSB(king_moves)));
        king_moves = king_moves ^ curr_move;
        // BISHOP/QUEEN
        uint64_t bishop_queen_mask = (white ? b->pieces[Black_Queen] : b->pieces[White_Queen]) | (white ? b->pieces[Black_Bishop] : b->pieces[White_Bishop]);
        Move bishop_moves = generateBishopMoves(&(Board){.pieces[White_Bishop] =curr_move, .pieces[Black_Bishop] = curr_move, 
                                                        .white_team_board = b->white_team_board, .black_team_board = b->black_team_board}, white)[0];
        uint64_t bishop_attack = bishop_moves.moves & bishop_queen_mask;
        if (bishop_moves.start_index < 0){
            bishop_attack = 0;
        }
        if (bishop_attack){
            king_moves_copy = king_moves_copy ^ curr_move;
            continue;
        }
        // ROOK/QUEEN
        uint64_t rook_queen_mask =  (white ? b->pieces[Black_Queen] : b->pieces[White_Queen]) | (white ? b->pieces[Black_Rook] : b->pieces[White_Rook]);
        Move rook_moves = generateRookMoves(&(Board){.pieces[White_Rook] = curr_move, .pieces[Black_Rook] = curr_move, 
                                                    .white_team_board = b->white_team_board, .black_team_board = b->black_team_board}, white)[0];
        uint64_t rook_attacks = rook_moves.moves & rook_queen_mask;
        if (rook_moves.start_index < 0){
            rook_attacks = 0;
        }
        if(rook_attacks){
            king_moves_copy = king_moves_copy ^ curr_move;
            continue;
        }
        // KINGHT
        uint64_t knight_attacks = knight_move_table[pos];
        uint64_t opp_knights = (white ? b->pieces[Black_Knight] : b->pieces[White_Knight]);
        knight_attacks &= opp_knights;
        if (knight_attacks){
            king_moves_copy = king_moves_copy ^ curr_move;
            continue;
        }
        // KING
        uint64_t king_attacks = king_move_table[pos];
        uint64_t opp_king_board = white ? b->pieces[Black_King] : b->pieces[White_King];
        uint64_t overlap = king_attacks & opp_king_board;
        if (overlap){
            king_moves_copy = king_moves_copy ^ curr_move;
        }
    }
    return king_moves_copy;
}


uint64_t generateCastles(Board *b, bool white){
    if (white && !(b->castle[King_W] | b->castle[Queen_W])){
        return 0;
    }
    else if (!white & !(b->castle[King_B] | b->castle[Queen_B])){
        return 0;
    }
    uint64_t board = white ? b->pieces[White_King] : b->pieces[Black_King];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    uint64_t moveboard = 0;
    
    if((b->castle[Queen_W] & white) | (!white & b->castle[Queen_B])){
        uint64_t queen_side_check = 0b11100;
        uint64_t queen_side_path = 0b11111;
        uint64_t queen_rook = 0b1;
        uint64_t team_XKR = ((team_board ^ board) ^ queen_rook) & queen_side_path;
        if(!white){
            queen_side_check = _byteswap_uint64(queen_side_check);
            queen_side_path = _byteswap_uint64(queen_side_path);
            queen_rook = _byteswap_uint64(queen_rook);
            team_XKR = ((team_board ^ board) ^ queen_rook) & queen_side_path;
        }
        
        if (!(team_XKR | (queen_side_path & opp_board)) & (valid_checks(b, white, queen_side_check) == queen_side_check)){
            moveboard |= 0b100;
        }
    }
    if((b->castle[King_W] & white) | (b->castle[King_B] & !white)){
        uint64_t king_side_check = 0b1110000;
        uint64_t king_side_path = 0b11110000; 
        uint64_t king_rook = 0b10000000;
        uint64_t teamXKR = ((team_board ^ board) ^ king_rook) & king_side_path;
        if(!white){
            king_side_check = _byteswap_uint64(king_side_check);
            king_side_path = _byteswap_uint64(king_side_path);
            king_rook = _byteswap_uint64(king_rook);
            teamXKR = ((team_board ^ board) ^ king_rook) & king_side_path;
        }

        // if theres no teammates in row | not opps in row
        if (!(teamXKR | (opp_board & king_side_path)) & (valid_checks(b, white, king_side_check) == king_side_check)){
            moveboard |= 0b1000000;
        }
    }
    
    return moveboard;
}

Move* generateKingMoves(Board *b, bool white){
    uint64_t board = white ? b->pieces[White_King] : b->pieces[Black_King];
    uint64_t team_board = white ? b->white_team_board : b->black_team_board;
    uint64_t opp_board = white ? b->black_team_board : b->white_team_board;
    int piece_count = __builtin_popcountll(board);
    // Normal, Kingside, queenside
    Move* moves = (Move*)calloc(3 , sizeof(Move));
    if (moves == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    if(piece_count ==0){
        moves[0].start_index = 65;
        moves[0].piece_type = white ? White_King : Black_King;
        return moves;
    }
    int pos = get_MSB(board);
    // cast moves from kings position
    uint64_t king_moves = king_move_table[pos];
    king_moves = (king_moves ^ team_board) & king_moves;
    king_moves = valid_checks(b, white, king_moves);
    uint64_t castling = generateCastles(b, white);
    if(castling){
        // Kingside
        if(0b1000000 & castling){
            moves[1].piece_type = white ? White_King : Black_King;
            moves[1].start_index = pos;
            moves[1].moves = white ? 0b1000000 : (1ULL << 62);
            moves[1].castle_side = white ? King_W : King_B;
        }
        else{
            moves[1].start_index = 65;
        }
        // Queenside
        if(0b100 & castling){
            moves[2].piece_type = white ? White_King : Black_King;
            moves[2].start_index = pos;
            moves[2].moves = white ? 0b100 : (1ULL << 58);
            moves[2].castle_side = white ? Queen_W : Queen_B;
        }
        else{
            moves[2].start_index = 65;
        }
    }

    moves[0].piece_type = white ? White_King : Black_King;
    moves[0].start_index = pos;
    moves[0].moves = king_moves;
    return moves;
}



void update_piece_board(Board *b, Piece p, int start_index, uint64_t end_index){
    b->pieces[p] = (b->pieces[p] ^ (start_index < 64? (1ULL << start_index) : 0)) | end_index;
    (p < 7) ? (b->white_team_board = ((b->white_team_board ^ (start_index < 64 ? (1ULL << start_index) : 0)) | end_index)) : (b->black_team_board = b->black_team_board ^ (start_index < 64? (1ULL << start_index) : 0) | end_index);
    return;
}

typedef struct {
    int curr_move;
    int move_count;
    Move* movelist;
} Move_Generator;

typedef struct {
    int curr_gen;
    bool team;
    Move_Generator* move_generator;
    Board* gen_board;
} Team_Generator;



Move_Generator* init_moveGen(Move* (*move_gen)(Board*, bool), Board *b, bool white, int move_count){
    Move_Generator *gen_pointer = (Move_Generator*)(calloc(1, sizeof(Move_Generator)));
    if (gen_pointer == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    gen_pointer->movelist=move_gen(b, white);
    gen_pointer->curr_move=0;
    gen_pointer->move_count=move_count;
    return gen_pointer;
}

Move get_next_move(Move_Generator* gen){
    while(gen->curr_move < gen->move_count){
        uint64_t possible_moves = gen->movelist[gen->curr_move].moves;
        if (!possible_moves){
            gen->curr_move++;
        }
        else
        {
            int pos = get_MSB(possible_moves);
            // printf("POS: %d\n", pos);
            gen->movelist[gen->curr_move].moves = possible_moves ^ (1ULL << pos);
            // printf("whats in transform %d\n", gen->movelist[gen->curr_move].piece_type);
            Piece trans = (gen->movelist[gen->curr_move].transform == White_King) ? gen->movelist[gen->curr_move].piece_type : gen->movelist[gen->curr_move].transform;
            return (Move){.start_index=gen->movelist[gen->curr_move].start_index, .moves=(1ULL << pos), .piece_type=gen->movelist[gen->curr_move].piece_type, 
                            .castle_side=gen->movelist[gen->curr_move].castle_side, .transform=trans};
        }
    }
    free(gen->movelist);
    return (Move){.piece_type=BLANK};
}


Team_Generator* init_teamGen(Board *b, bool team){
    Team_Generator *team_gens = (Team_Generator*)(calloc(1, sizeof(Team_Generator)));
    if (team_gens == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    team_gens->move_generator = init_moveGen(generateKingMoves, b, team, 3);
    team_gens->curr_gen = 0;
    team_gens->team = team;
    team_gens->gen_board = b;
    return team_gens;
}
typedef Move* (*MoveGenFunctions)(Board*, bool);
static MoveGenFunctions move_funks[] = {generateKingMoves, generateQueenMoves, generateRookMoves, generateBishopMoves, generateKnightMoves, generatePawnMoves};
static int MoveFuncCounts[] = {2, 1, 1, 1, 1, 5};

Move next_move(Team_Generator* team_gen){
    while(team_gen->curr_gen < 6){
        Move_Generator curr_gen = (team_gen->move_generator[0]);
        Move m = get_next_move(&curr_gen);
        if (m.piece_type != BLANK){
            return m;
        }
        else{
            team_gen->curr_gen++;
            if(team_gen->curr_gen >=  6){
                break;
            }
            Move_Generator *new_gen = init_moveGen(move_funks[team_gen->curr_gen], team_gen->gen_board, team_gen->team, ((MoveFuncCounts[team_gen->curr_gen] == 1) ?  __builtin_popcountll(team_gen->gen_board->pieces[team_gen->curr_gen]) : (MoveFuncCounts[team_gen->curr_gen])));
            team_gen->move_generator = new_gen;
        }
    }
    free(team_gen->move_generator);
    return (Move){.piece_type=BLANK};
}


void do_move(Move *m, Board *b){
    int end_index = get_MSB(m->moves);
    Piece taken = b->mailbox[end_index];
    m->taken = taken;
    if (taken != BLANK){
        update_piece_board(b, taken, end_index, 0);
        
    }
    b->mailbox[end_index] = m->transform;
    switch(m->piece_type){
        case White_Pawn:
            if(m->start_index == Alt_2){
                b->en_passant_counter++;
                b->en_passant[b->en_passant_counter] = (m->moves << 8);
            }
            m->start_index = calc_pawn_start(m->start_index, ((m->piece_type) < 6), end_index);
            if(m->moves > 72057594037927936){
                    m->transform = White_Queen;
                    update_piece_board(b, White_Queen, 65, m->moves);
                    update_piece_board(b, m->piece_type, m-> start_index, 0);
                }
            break;
        case Black_Pawn:
            if(m->start_index == Alt_2){
                b->en_passant_counter++;
                b->en_passant[b->en_passant_counter] = (m->moves << 8);
            }
            m->start_index = calc_pawn_start(m->start_index, ((m->piece_type) < 6), end_index);
            if(m->moves <= 255){
                    m->transform = Black_Queen;
                    update_piece_board(b, Black_Queen, 65, m->moves);
                    b->mailbox[m->start_index] = BLANK;
                    update_piece_board(b, m->piece_type, m-> start_index, 0);
                    return;
                }
            break;
        case White_King:
        case Black_King:
            if(m->castle_side < 4){
                switch (m->castle_side){
                    // just need to move the rooks
                    case King_W:
                        b->mailbox[7] = BLANK;
                        b->mailbox[5] = White_Rook;
                        update_piece_board(b, White_Rook, 7, 0b100000);
                        break;
                    case Queen_W:
                        b->mailbox[0] = BLANK;
                        b->mailbox[3] = White_Rook;
                        update_piece_board(b, White_Rook, 7, 0b1000);
                        break;
                    case King_B:
                        b->mailbox[63] = BLANK;
                        b->mailbox[61] = Black_Rook;
                        update_piece_board(b, Black_Rook, 63, (1ULL << 61));
                        break;
                    case Queen_B:
                        b->mailbox[56] = BLANK;
                        b->mailbox[59] = Black_Rook;
                        update_piece_board(b, Black_Rook, 56, (1ULL << 59));
                        break;
                }
            }
            break;
    }
    update_piece_board(b, m->piece_type, m->start_index, m->moves);
    b->mailbox[m->start_index] = BLANK;
    return;
}

void undo_move(Move *m, Board *b){
    int end_index = get_MSB(m->moves);
    b->mailbox[m->start_index] = m->piece_type;
    b->mailbox[end_index] = m->taken;
    if(m->taken != BLANK){
        update_piece_board(b, m->taken, 65, m->moves);
    }
    //special move cases
    switch (m->piece_type){
        case White_Pawn:
        case Black_Pawn:
            b->en_passant_counter--;
            if(m->transform != m->piece_type){
                update_piece_board(b, m->transform, end_index, 0);
                update_piece_board(b, m->piece_type, 65, (1ULL << m->start_index));
                 return;
            }
            break;

        case White_King:
        case Black_King:
            if(m->castle_side < 4){
                switch (m->castle_side){
                    // just need to unmove the rooks
                    case King_W:
                        b->mailbox[5] = BLANK;
                        b->mailbox[7] = White_Rook;
                        update_piece_board(b, White_Rook, 5, 0b10000000);
                        break;
                    case Queen_W:
                        b->mailbox[3] = BLANK;
                        b->mailbox[0] = White_Rook;
                        update_piece_board(b, White_Rook, 3, 0b1);
                        break;
                    case King_B:
                        b->mailbox[61] = BLANK;
                        b->mailbox[63] = Black_Rook;
                        update_piece_board(b, Black_Rook, 61, (1ULL << 63));
                        break;
                    case Queen_B:
                        b->mailbox[59] = BLANK;
                        b->mailbox[56] = Black_Rook;
                        update_piece_board(b, Black_Rook, 59, (1ULL << 56));
                        break;
                }
            }

    }
    update_piece_board(b, m->piece_type, end_index, (1ULL << m->start_index));
}


int eval(){
    return 0;
}
Move DFS_MAX(int curr_depth, int cut_off, int alpha, int beta, Board *b);
Move DFS_MIN(int curr_depth, int cut_off, int alpha, int beta, Board *b);



Move DFS_MAX(int curr_depth, int cut_off, int alpha, int beta, Board *b){
    if (curr_depth >= cut_off){
        Move m = (Move){.eval=eval()};
        return m;
    }
    Move bestMove;
    Team_Generator *white = init_teamGen(b, true);
    Move m = next_move(white);
    while(m.piece_type != BLANK){
        do_move(&m, b);
        Move min = DFS_MIN(curr_depth+1, cut_off, alpha, beta, b);
        if (min.eval >= alpha){
            alpha = min.eval;
            bestMove = m;
            if (alpha > beta){
                undo_move(&m, b);
                break;
            }
        }
        undo_move(&m, b);
    }
    return bestMove;
}
Move DFS_MIN(int curr_depth, int cut_off, int alpha, int beta, Board *b){
    if (curr_depth >= cut_off){
        Move m = (Move){.eval=eval()};
        return m;
    }
    Move bestMove;
    Team_Generator *black = init_teamGen(b, false);
    Move m = next_move(black);
    while(m.piece_type != BLANK){
        do_move(&m, b);
        Move max = DFS_MAX(curr_depth+1, cut_off, alpha, beta, b);
        if (max.eval < beta){
            alpha = max.eval;
            bestMove = m;
            if (alpha > beta){
                undo_move(&m, b);
                break;
            }
        }
        undo_move(&m, b);
    }
    return bestMove;
}


Board init_board(){
    Board b = {
        .pieces[White_King] = 0b10000, //0b10000
        .pieces[White_Queen] = 0b1000, //0b1000
        .pieces[White_Rook] = 0b10000001, //0b10000001
        .pieces[White_Bishop] = 0b00100100, //0b100100
        .pieces[White_Knight] = 0b01000010, //0b0100001
        .pieces[White_Pawn] = 0b1111111100000000,
        .pieces[Black_King] = 1ULL << 60,
        .pieces[Black_Queen] = 1ULL << 59,
        .pieces[Black_Rook] = 129ULL << 56,
        .pieces[Black_Bishop] = 36ULL << 56,
        .pieces[Black_Knight] = 66ULL << 56,
        .pieces[Black_Pawn] = 255ULL << 48,
        .en_passant_counter = 0,
        .en_passant = (uint64_t*)(calloc(300, sizeof(uint64_t))),
        .castle = {'K', 'Q', 'k', 'q'}
        };
    uint64_t white_team_board = 0;
    for(int i =0; i < 6; i++){
        white_team_board |= b.pieces[i];
    }
    uint64_t black_team_board = 0;
    for(int i =6; i < 12; i++){
        black_team_board |= b.pieces[i];
    }
    for(int i =0; i < 64; i++){
        b.mailbox[i] = BLANK;
    }
    for(int i = 0; i < 12; i++){
        uint64_t curr_board = b.pieces[i];
        while (curr_board){
            int pos = get_MSB(curr_board);
            curr_board = curr_board ^ (1ULL << pos);
            b.mailbox[pos] = i;
        }
    }
    b.white_team_board = white_team_board;
    b.black_team_board = black_team_board;
    return b;
}

int main() {
    setlocale(LC_ALL, ""); 
    Board b = init_board();
    print_board(&b);
    Move m = DFS_MAX(0, 2, INT_MIN, INT_MAX, &b);
    do_move(&m, &b);
    print_board(&b);
    return 0;
}

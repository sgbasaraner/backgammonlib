use crate::game::{
    DiceRoll, DiceValue, Game, GameState, HitStatus, Move, MoveError, MovePosition, MoveTuple, Play,
};
use std::num::ParseIntError;

enum GameLineParseResult {
    NsTuple((NameScoreTuple, NameScoreTuple)),
    MoveTuple(MoveTuple),
    String(String),
}

fn parse_game_line(line: &str, line_no: usize) -> Result<GameLineParseResult, ParsingError> {
    if let Ok(mt) = parse_move_tuple(line, line_no) {
        return Ok(GameLineParseResult::MoveTuple(mt));
    }

    if let Ok(sb) = parse_scoreline(line, line_no) {
        return Ok(GameLineParseResult::NsTuple(sb));
    }

    Ok(GameLineParseResult::String(line.to_owned()))
}

struct NameScoreTuple {
    name: String,
    score: usize,
}

fn parse_scoreline(
    line: &str,
    line_no: usize,
) -> Result<(NameScoreTuple, NameScoreTuple), ParsingError> {
    let split: Vec<&str> = line.split_whitespace().collect();
    if split.len() != 6 {
        Err(ParsingError {
            error_type: ParsingErrorType::UnexpectedLine,
            line_no,
            line_pos: 0,
        })?
    }

    if split[1] != split[4] || split[1] != ":" {
        Err(ParsingError {
            error_type: ParsingErrorType::UnexpectedLine,
            line_no,
            line_pos: 0,
        })?
    }

    let score_1_parsed: usize = split[2].parse().map_err(|_e| ParsingError {
        error_type: ParsingErrorType::UnexpectedLine,
        line_no,
        line_pos: 0,
    })?;

    let score_2_parsed: usize = split[5].parse().map_err(|_e| ParsingError {
        error_type: ParsingErrorType::UnexpectedLine,
        line_no,
        line_pos: 0,
    })?;

    Ok((
        NameScoreTuple {
            name: split[0].to_string(),
            score: score_1_parsed,
        },
        NameScoreTuple {
            score: score_2_parsed,
            name: split[3].to_string(),
        },
    ))
}

#[derive(Clone, Debug)]
pub enum ParsingErrorType {
    NoMoveNumber,
    ExpectedInteger(ParseIntError),
    MoveError(MoveError),
    NoDice,
    DiceNotInteger,
    InvalidWinsString,
    InvalidDice,
    InvalidPlay,
    InvalidPosition,
    InvalidPlayEnd,
    InvalidPlayStart,
    InvalidDiceSeparator,
    InvalidDealMove,
    UnexpectedLine,
    EmptyString,
}

#[derive(Clone, Debug)]
pub struct ParsingError {
    pub error_type: ParsingErrorType,
    pub line_no: usize,
    pub line_pos: usize,
}

impl ParsingError {
    pub fn from_move_error(move_error: MoveError, line_no: usize) -> ParsingError {
        ParsingError {
            error_type: ParsingErrorType::MoveError(move_error),
            line_no,
            line_pos: 0,
        }
    }
}

pub fn parse_move_tuple(source_line: &str, line_no: usize) -> Result<MoveTuple, ParsingError> {
    let move_no_separator_start_location =
        search_forwards_until_match(source_line, 0, |c| c == ')').ok_or(ParsingError {
            error_type: ParsingErrorType::NoMoveNumber,
            line_no: line_no,
            line_pos: 0,
        })?;

    let lookup_idx = move_no_separator_start_location - 1;
    let start_pos =
        search_backwards_until_match(source_line, lookup_idx, |c| c.is_ascii_whitespace())
            .map(|ws_idx| ws_idx + 1)
            .unwrap_or(0);

    let move_no = collect_chars(
        source_line,
        start_pos,
        Some(move_no_separator_start_location),
    )
    .parse::<usize>()
    .map_err(|e| ParsingError {
        error_type: ParsingErrorType::ExpectedInteger(e),
        line_no: line_no,
        line_pos: start_pos,
    })?;

    let d1 = parse_next_dice(source_line, move_no_separator_start_location + 1, line_no).ok();
    match d1 {
        Some(d1) => {
            match parse_next_dice(source_line, d1.start_idx + 3, line_no) {
                Ok(d2) => {
                    let d1_moves_start = d1.start_idx + 3;
                    let d1_moves_end = d2.start_idx;

                    let p1_moves = collect_chars(source_line, d1_moves_start, Some(d1_moves_end));
                    let p1_moves = parse_moves(
                        &p1_moves,
                        line_no,
                        DiceRoll {
                            dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                            dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                        },
                    )?;

                    let mut mt = MoveTuple {
                        move_1: Some(p1_moves),
                        move_2: None,
                        move_no: move_no,
                    };

                    // parse d2 moves

                    let d2_moves_start = d2.start_idx + 3;
                    let p2_moves = collect_chars(source_line, d2_moves_start, None);
                    mt.move_2 = Some(parse_moves(
                        &p2_moves,
                        line_no,
                        DiceRoll {
                            dice_1: DiceValue::try_from(d2.dice_1).unwrap(),
                            dice_2: DiceValue::try_from(d2.dice_2).unwrap(),
                        },
                    )?);

                    Ok(mt)
                }
                Err(_) => {
                    // only one dice. this means it's either:
                    // 1. takes followed by dice
                    // 2. dice followed by doubles
                    // 3. dice followed by nothing
                    let collected =
                        collect_chars(source_line, move_no_separator_start_location + 1, None);
                    let collected_split: Vec<&str> = collected.split_ascii_whitespace().collect();
                    match *collected_split.first().unwrap() {
                        "Takes" => {
                            // 1. takes followed by dice

                            let d1_moves_start = d1.start_idx + 3;
                            let p1_moves = collect_chars(source_line, d1_moves_start, None);
                            let p1_moves = parse_moves(
                                &p1_moves,
                                line_no,
                                DiceRoll {
                                    dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                    dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                },
                            )?;

                            Ok(MoveTuple {
                                move_1: Some(Move::Take),
                                move_2: Some(p1_moves),
                                move_no: move_no,
                            })
                        }
                        _ => {
                            if collected_split.len() >= 3 {
                                // 2. dice followed by doubles
                                let parse_error = ParsingError {
                                    line_no: line_no,
                                    line_pos: move_no_separator_start_location + 1,
                                    error_type: ParsingErrorType::InvalidDealMove,
                                };

                                let first_item = collected_split
                                    .get(collected_split.len() - 3)
                                    .ok_or(parse_error.clone())?;
                                let second_item = collected_split
                                    .get(collected_split.len() - 2)
                                    .ok_or(parse_error.clone())?;

                                if *first_item == "Doubles" && *second_item == "=>" {
                                    let third_item =
                                        collected_split.last().ok_or(parse_error.clone())?;
                                    let multiplier: usize =
                                        third_item.parse().ok().ok_or(parse_error.clone())?;
                                    // we now know the doubles value. parse dice play and return

                                    let d1_moves_start = d1.start_idx + 3;
                                    let d1_moves_end = search_forwards_until_match(
                                        source_line,
                                        d1.start_idx + 3,
                                        |c| c == 'D',
                                    )
                                    .unwrap();
                                    let p1_moves = collect_chars(
                                        source_line,
                                        d1_moves_start,
                                        Some(d1_moves_end),
                                    );
                                    let p1_moves = parse_moves(
                                        &p1_moves,
                                        line_no,
                                        DiceRoll {
                                            dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                            dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                        },
                                    )?;

                                    return Ok(MoveTuple {
                                        move_1: Some(p1_moves),
                                        move_2: Some(Move::Double(multiplier)),
                                        move_no: move_no,
                                    });
                                }
                            }

                            // 3. dice followed by nothing
                            let d1_moves_start = d1.start_idx + 3;
                            let p1_moves = collect_chars(source_line, d1_moves_start, None);
                            let p1_moves = parse_moves(
                                &p1_moves,
                                line_no,
                                DiceRoll {
                                    dice_1: DiceValue::try_from(d1.dice_1).unwrap(),
                                    dice_2: DiceValue::try_from(d1.dice_2).unwrap(),
                                },
                            )?;

                            Ok(MoveTuple {
                                move_1: Some(p1_moves),
                                move_2: None,
                                move_no: move_no,
                            })
                        }
                    }
                }
            }
        }
        None => {
            let collected = collect_chars(source_line, move_no_separator_start_location + 1, None);
            let collected_split: Vec<&str> = collected.split_ascii_whitespace().collect();

            let parse_error = ParsingError {
                line_no: line_no,
                line_pos: move_no_separator_start_location + 1,
                error_type: ParsingErrorType::InvalidDealMove,
            };

            let split_len = collected_split.len();

            match split_len {
                1 => match *collected_split.get(0).unwrap() {
                    "Takes" => Ok(MoveTuple {
                        move_1: Some(Move::Take),
                        move_2: None,
                        move_no: move_no,
                    }),
                    "Drops" => Ok(MoveTuple {
                        move_1: Some(Move::Drop),
                        move_2: None,
                        move_no: move_no,
                    }),
                    _ => Err(parse_error),
                },
                3 => {
                    let third_item = collected_split.get(2).ok_or(parse_error.clone())?;
                    let multiplier: usize = third_item.parse().ok().ok_or(parse_error.clone())?;
                    let first_item = collected_split.get(0).ok_or(parse_error.clone())?;
                    let second_item = collected_split.get(1).ok_or(parse_error.clone())?;

                    if *first_item != "Doubles" || *second_item != "=>" {
                        Err(parse_error.clone())
                    } else {
                        Ok(MoveTuple {
                            move_1: Some(Move::Double(multiplier)),
                            move_2: None,
                            move_no: move_no,
                        })
                    }
                }
                4 => {
                    let first_item = collected_split.get(0).ok_or(parse_error.clone())?;

                    // TODO: remove when proper wins parsing is implemented
                    if *first_item == "Drops" {
                        return Ok(MoveTuple {
                            move_1: Some(Move::Drop),
                            move_2: None,
                            move_no: move_no,
                        });
                    }

                    let third_item = collected_split.get(2).ok_or(parse_error.clone())?;
                    let multiplier: usize = third_item.parse().ok().ok_or(parse_error.clone())?;
                    let second_item = collected_split.get(1).ok_or(parse_error.clone())?;

                    if *first_item != "Doubles" || *second_item != "=>" {
                        Err(parse_error.clone())
                    } else {
                        match *collected_split.get(3).unwrap() {
                            "Takes" => Ok(MoveTuple {
                                move_1: Some(Move::Double(multiplier)),
                                move_2: Some(Move::Take),
                                move_no: move_no,
                            }),
                            "Drops" => Ok(MoveTuple {
                                move_1: Some(Move::Double(multiplier)),
                                move_2: Some(Move::Drop),
                                move_no: move_no,
                            }),
                            _ => Err(parse_error),
                        }
                    }
                }
                _ => Err(parse_error),
            }
        }
    }
}

pub fn parse_moves(line: &str, line_no: usize, dice_roll: DiceRoll) -> Result<Move, ParsingError> {
    let mut plays: Vec<Play> = Vec::new();
    let split = line.trim().split_ascii_whitespace();

    for play_candidate in split {
        let ends_critical = play_candidate.ends_with("*");
        let valid_ending = ends_critical
            || (play_candidate.chars().last().is_some()
                && play_candidate.chars().last().unwrap().is_ascii_digit());

        if !valid_ending {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlayEnd,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let valid_start = play_candidate.chars().nth(0).is_some()
            && play_candidate.chars().nth(0).unwrap().is_ascii_digit();

        if !valid_start {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlayStart,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let mut source_target_split = play_candidate.split("/");

        let source_str = source_target_split.next();
        let target_str = source_target_split.next();

        if source_target_split.next().is_some() || source_str.is_none() || target_str.is_none() {
            return Err(ParsingError {
                error_type: ParsingErrorType::InvalidPlay,
                line_no: line_no,
                line_pos: 0,
            });
        }

        let source_int: Option<u8> = source_str.unwrap().parse().ok();
        let target_int: Option<u8> = if ends_critical {
            let mut target_owned = target_str.unwrap().to_owned();
            target_owned.truncate(target_owned.len() - 1);
            target_owned.parse().ok()
        } else {
            target_str.unwrap().parse().ok()
        };

        for i in [source_int, target_int] {
            if i.is_none() || i.unwrap() > 25 {
                return Err(ParsingError {
                    error_type: ParsingErrorType::InvalidPosition,
                    line_no: line_no,
                    line_pos: 0,
                });
            }
        }

        plays.push(Play {
            hit_status: if ends_critical {
                HitStatus::Hit
            } else {
                HitStatus::NoHit
            },
            from: MovePosition::from_board_number(source_int.unwrap()).unwrap(),
            to: MovePosition::from_board_number(target_int.unwrap()).unwrap(),
        });
    }
    if plays.is_empty() {
        Ok(Move::CantPlay(dice_roll))
    } else {
        Ok(Move::Plays((dice_roll, plays)))
    }
}
pub struct GameParser {
    game: Game,
}

impl GameParser {
    pub fn new() -> Self {
        Self { game: Game::new() }
    }

    fn build_parsing(
        &mut self,
        res: GameLineParseResult,
        line_no: usize,
    ) -> Result<(), ParsingError> {
        match res {
            GameLineParseResult::NsTuple(nst) => {
                if self.game.p1_name.is_some()
                    || self.game.p2_name.is_some()
                    || self.game.p1_points.is_some()
                    || self.game.p2_points.is_some()
                {
                    Err(ParsingError {
                        error_type: ParsingErrorType::UnexpectedLine,
                        line_no,
                        line_pos: 0,
                    })
                } else {
                    self.game.p1_name = Some(nst.0.name);
                    self.game.p2_name = Some(nst.1.name);
                    self.game.p1_points = Some(nst.0.score);
                    self.game.p2_points = Some(nst.1.score);
                    Ok(())
                }
            }
            GameLineParseResult::MoveTuple(mt) => {
                if self.game.state.past_moves.len() == 1
                    && self.game.state.past_moves.last().unwrap().move_2.is_none()
                {
                    // apply corrections TODO: remove this hack after implementing proper single move parsing
                    let new_first_move = self.game.state.past_moves.pop().unwrap().move_1.unwrap();
                    let mut new_state = GameState::new();
                    new_state
                        .add_move(Move::Skip)
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                    new_state
                        .add_move(new_first_move)
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                    self.game.state = new_state;
                }

                for mov in mt.moves().into_iter() {
                    self.game
                        .state
                        .add_move(mov.clone())
                        .map_err(|ge| ParsingError::from_move_error(ge, line_no))?;
                }

                Ok(())
            }
            GameLineParseResult::String(gn) => {
                if self.game.name.is_some() {
                    Err(ParsingError {
                        error_type: ParsingErrorType::UnexpectedLine,
                        line_no,
                        line_pos: 0,
                    })
                } else {
                    self.game.name = Some(gn);
                    Ok(())
                }
            }
        }
    }

    pub fn parse(
        &mut self,
        source_lines: Vec<&str>,
        start_line_idx: usize,
    ) -> Result<Game, ParsingError> {
        let res = match source_lines.len() {
            0 => Err(ParsingError {
                error_type: ParsingErrorType::EmptyString,
                line_no: start_line_idx,
                line_pos: 0,
            }),
            _ => {
                for (i, line) in source_lines.iter().enumerate() {
                    let idx = start_line_idx + i;
                    // TODO: start parsing wins lines in the future
                    if !line.trim().starts_with("Wins") {
                        let parsed = parse_game_line(line, idx)?;

                        let err = self.build_parsing(parsed, idx);
                        if err.is_err() {
                            self.game = Game::new();
                            err?;
                        }
                    }
                }

                Ok(std::mem::take(&mut self.game))
            }
        };

        self.game = Game::new();

        res
    }
}

pub fn parse_next_dice(
    line: &str,
    start_idx: usize,
    line_no: usize,
) -> Result<ParsedDice, ParsingError> {
    let move_separator_address =
        search_forwards_until_match(line, start_idx, |c| c == ':').ok_or(ParsingError {
            error_type: ParsingErrorType::NoDice,
            line_no: line_no,
            line_pos: 0,
        })?;
    let dice_1_val_address = move_separator_address - 2;

    let dice_1 = check_dice_at_address(line, line_no, dice_1_val_address)?;

    let dice_2_val_address = move_separator_address - 1;

    let dice_2 = check_dice_at_address(line, line_no, dice_2_val_address)?;

    let parse_err = ParsingError {
        error_type: ParsingErrorType::InvalidDiceSeparator,
        line_no: line_no,
        line_pos: dice_2_val_address,
    };
    let move_separator_check = line
        .chars()
        .nth(move_separator_address)
        .map(|c| {
            if c == ':' {
                Ok(())
            } else {
                Err(parse_err.clone())
            }
        })
        .ok_or(parse_err)
        .and_then(|f| f);
    move_separator_check?;

    Ok(ParsedDice {
        start_idx: dice_1_val_address,
        dice_1,
        dice_2,
    })
}

struct ParsedDice {
    start_idx: usize,
    dice_1: u8,
    dice_2: u8,
}

fn check_dice_at_address(
    source_line: &str,
    line_no: usize,
    address: usize,
) -> Result<u8, ParsingError> {
    let dice_2_check: Result<u8, ParsingError> = source_line
        .chars()
        .nth(address)
        .ok_or(ParsingError {
            error_type: ParsingErrorType::NoDice,
            line_no: line_no,
            line_pos: address,
        })
        .and_then(|c| {
            is_valid_dice(&c).ok_or(ParsingError {
                error_type: ParsingErrorType::DiceNotInteger,
                line_no: line_no,
                line_pos: address,
            })
        });

    dice_2_check
}

fn is_valid_dice(char: &char) -> Option<u8> {
    if !char.is_ascii_digit() {
        return None;
    }

    let mut s = String::new();
    s.push(char.clone());
    let int = s.parse::<u8>().unwrap();
    if int >= 1 && int <= 6 {
        return Some(int);
    } else {
        None
    }
}
fn search_backwards_until_match<F>(line: &str, start_idx: usize, matcher: F) -> Option<usize>
where
    F: FnOnce(char) -> bool + Copy,
{
    let mut idx = start_idx;

    while let Some(char) = line.chars().nth(idx) {
        let value = matcher(char);
        if value {
            return Some(idx);
        }
        if idx == 0 {
            return None;
        }
        idx -= 1;
    }

    None
}

fn search_forwards_until_match<F>(line: &str, start_idx: usize, matcher: F) -> Option<usize>
where
    F: FnOnce(char) -> bool + Copy,
{
    let mut idx = start_idx;

    while let Some(char) = line.chars().nth(idx) {
        let value = matcher(char);
        if value {
            return Some(idx);
        }
        idx += 1;
    }

    None
}

fn collect_chars(line: &str, from_inc: usize, to_exc: Option<usize>) -> String {
    let mut collector = String::new();
    let mut idx = from_inc;
    while let Some(char) = line.chars().nth(idx) {
        if to_exc.is_some() && idx >= to_exc.unwrap() {
            break;
        }
        collector.push(char);
        idx += 1;
    }

    collector
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::GameParser;
    use std::fs;

    #[test]
    fn parse_match_1_strs() {
        let path = "test_match_1.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    #[test]
    fn parse_match_2_strs() {
        let path = "test_match_2.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    #[test]
    fn parse_match_3_strs() {
        let path = "test_match_3.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    #[test]
    fn parse_match_4_strs() {
        let path = "test_match_4.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    #[test]
    fn parse_match_5_strs() {
        let path = "test_match_5.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    #[test]
    fn parse_match_6_strs() {
        let path = "test_match_6.txt";
        let fc = fs::read_to_string(path).expect("Could not read file");
        let match_strs = parse_match_strings(&fc);

        for game_lines in match_strs {
            let parsed = GameParser::new().parse(game_lines, 0);
            assert!(parsed.is_ok());
        }
    }

    fn parse_match_strings(contents: &str) -> Vec<Vec<&str>> {
        let mut this_game = Vec::<&str>::new();
        let mut all_games = Vec::new();

        for line in contents.lines().skip(2) {
            if line.trim().is_empty() {
                if !this_game.is_empty() {
                    all_games.push(std::mem::take(&mut this_game));
                }
            } else {
                this_game.push(line);
            }
        }

        if !this_game.is_empty() {
            all_games.push(this_game);
        }

        all_games
    }

    fn parse_game(str: &str) {
        let lines = str.lines().collect();
        let parsed = GameParser::new().parse(lines, 0);
        assert!(parsed.is_ok());
    }

    #[test]
    fn parse_game_ex() {
        let game = "Game 1
 Mochizuki_Masayouki_(JPN) : 0  Takumitsu_Suzuki_(JPN) : 0
  1)                             32: 24/21 13/11
  2) 52: 13/8 6/4*               54: 25/21* 13/8
  3) 43: 25/21 24/21             53: 24/21 13/8
  4) 62: 13/7 7/5                64: 21/15 15/11
  5) 44: 13/9 9/5 8/4* 8/4       21: 25/23 23/22
  6)  Doubles => 2                Takes
  7) 21: 6/4 4/3*                32: 25/22* 8/6
  8) 32: 25/23 6/3*              21: 25/23 23/22*
  9) 54: 25/20 24/20             33: 13/10 13/10 6/3 6/3
 10) 62: 13/7 7/5                53: 11/8 11/6
 11) 22: 23/21 13/11 11/9 5/3*   21: 25/23 23/22*
 12) 31: 25/24 21/18             54: 22/18 18/13
 13) 66: 20/14 20/14 18/12* 14/8 44:
 14) 64: 14/10 12/6              54:
 15) 31: 10/7 8/7                51: 25/24 8/3
 16) 42: 24/20 20/18             33: 10/7* 10/7 8/5 8/5
 17) 33:                         66: 8/2 8/2 7/1 7/1
 18) 51:                         64: 6/2
 19) 54: 25/21 21/16             61: 24/23
 20) 53: 21/18 21/16             21: 23/22 6/4
 21) 43: 16/12 12/9              31: 6/5 4/1
 22) 31: 16/15 6/3*              55:
 23) 64: 9/5 9/3                 62: 25/23
 24) 54: 15/10 10/6              65: 6/1
 25) 54: 18/13 6/2*              22: 25/23* 6/4 5/3 4/2
 26) 22:                         64: 5/1
 27) 63: 25/19 5/2*              41: 25/24 5/1
 28) 33: 13/10 10/7 8/5 5/2      55:
 29) 54: 19/14 14/10             62: 3/1
 30) 21: 10/9 8/6                42: 3/1
 31) 61: 9/8 7/1*                64:
 32) 33: 8/5 7/4 7/4 4/1         61:
 33) 61: 6/0 5/4                 33:
 34) 52: 6/4 6/1                 62: 25/19 19/17
 35) 32: 3/0 2/0                 61: 17/11 11/10
 36) 11: 4/3 1/0 1/0 1/0         31: 10/7 7/6
 37) 63: 5/0 3/0                 33: 6/3 3/0 3/0 3/0
 38) 62: 5/0 2/0                 61: 2/0 1/0
 39) 11: 4/3 3/2 2/1 1/0         65: 2/0 2/0
 40) 61: 4/0 3/2                 64: 2/0 1/0
 41) 61: 4/0 2/1                 51: 1/0 1/0
 42) 22: 4/2 2/0 1/0
      Wins 2 points";

        parse_game(game);
    }

    #[test]
    fn move_tuple_standard() {
        let str = "18) 43: 25/21 13/10             53: 6/3 6/1 ";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 18);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Four,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Ten
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Six,
                        to: MovePosition::Three
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Six,
                        to: MovePosition::One
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_tuple_hit() {
        let str = "2) 52: 13/8 6/4*               54: 25/21* 13/8";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 2);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Six,
                        to: MovePosition::Four
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Four
                },
                vec![
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_tuple_doubles_takes() {
        let str = "  6)  Doubles => 2                Takes";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 6);
        assert_eq!(parsed.move_1, Some(Move::Double(2)),);
        assert_eq!(parsed.move_2, Some(Move::Take));
    }

    #[test]
    fn move_quadruple_and_noplay() {
        let str = " 13) 66: 20/14 20/14 18/12* 14/8 44: ";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 13);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Six,
                    dice_2: DiceValue::Six
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Twenty,
                        to: MovePosition::Fourteen
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Twenty,
                        to: MovePosition::Fourteen
                    },
                    Play {
                        hit_status: HitStatus::Hit,
                        from: MovePosition::Eighteen,
                        to: MovePosition::Twelve
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Fourteen,
                        to: MovePosition::Eight
                    },
                ]
            )))
        );
        assert_eq!(
            parsed.move_2,
            Some(Move::CantPlay(DiceRoll {
                dice_1: DiceValue::Four,
                dice_2: DiceValue::Four
            }))
        );
    }

    #[test]
    fn first_move_parse() {
        let str = "  1)                             32: 24/21 13/11 ";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 1);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Three,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::TwentyFour,
                        to: MovePosition::TwentyOne
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eleven
                    },
                ]
            )))
        );
        assert_eq!(parsed.move_2, None,);
    }

    #[test]
    fn move_take_and_play() {
        let str = "8)  Takes                      53: 13/8 10/7";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 8);
        assert_eq!(parsed.move_1, Some(Move::Take));
        assert_eq!(
            parsed.move_2,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Five,
                    dice_2: DiceValue::Three
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Thirteen,
                        to: MovePosition::Eight
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Ten,
                        to: MovePosition::Seven
                    },
                ]
            )))
        );
    }

    #[test]
    fn move_double_drop() {
        let str = "11)  Doubles => 2                Drops";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 11);
        assert_eq!(parsed.move_1, Some(Move::Double(2)));
        assert_eq!(parsed.move_2, Some(Move::Drop));
    }

    #[test]
    fn move_then_doubles() {
        let str = " 7) 42: 25/23 25/21              Doubles => 2";
        let parsed = parse_move_tuple(str, 1);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.move_no, 7);
        assert_eq!(
            parsed.move_1,
            Some(Move::Plays((
                DiceRoll {
                    dice_1: DiceValue::Four,
                    dice_2: DiceValue::Two
                },
                vec![
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyThree
                    },
                    Play {
                        hit_status: HitStatus::NoHit,
                        from: MovePosition::Bar,
                        to: MovePosition::TwentyOne
                    },
                ]
            )))
        );
        assert_eq!(parsed.move_2, Some(Move::Double(2)));
    }
}

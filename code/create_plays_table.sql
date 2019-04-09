DROP TABLE IF EXISTS retrosheet.plays;
CREATE TABLE retrosheet.plays (
  game_id VARCHAR(12) NOT NULL,
  event_id SMALLINT NOT NULL,
  inning TINYINT NOT NULL,                    -- 1 or higher
  batting_team BOOLEAN NOT NULL,              -- 0=visitor, 1=home
  first_runner VARCHAR(8) DEFAULT NULL,
  second_runner VARCHAR(8) DEFAULT NULL,
  third_runner VARCHAR(8) DEFAULT NULL,
  outs TINYINT NOT NULL,                      -- 0, 1, or 2
  batter_event_flag ENUM('T','F') NOT NULL,   -- event terminated batter's appearance?
  batter_dest TINYINT UNSIGNED,               -- 0-4, 5 if scores and unearned, 6 if scores team unearned
  runner_on_1st_dest TINYINT UNSIGNED,        -- 0-4, 5 if scores and unearned, 6 if scores team unearned
  runner_on_2nd_dest TINYINT UNSIGNED,        -- 0-4, 5 if scores and unearned, 6 if scores team unearned
  runner_on_3rd_dest TINYINT UNSIGNED,        -- 0-4, 5 if scores and unearned, 6 if scores team unearned
  outs_on_play TINYINT NOT NULL,              -- 0-3
  old_1st VARCHAR(8) DEFAULT NULL,  
  old_2nd VARCHAR(8) DEFAULT NULL,  
  old_3rd VARCHAR(8) DEFAULT NULL,  
  old_outs VARCHAR(8) DEFAULT NULL,  
  play_flag VARCHAR(8) DEFAULT NULL,  
  new_1st VARCHAR(8) DEFAULT NULL,  
  new_2nd VARCHAR(8) DEFAULT NULL,  
  new_3rd VARCHAR(8) DEFAULT NULL,  
  new_outs VARCHAR(8) DEFAULT NULL,  
  old_state VARCHAR(16) DEFAULT NULL,
  new_state VARCHAR(16) DEFAULT NULL,
  transition VARCHAR(16) DEFAULT NULL,
  PRIMARY KEY (game_id,event_id)
) ENGINE=MyISAM;
INSERT INTO retrosheet.plays (
  game_id,
  event_id,
  inning,
  batting_team,
  first_runner,
  second_runner,
  third_runner,
  outs,
  batter_event_flag,
  batter_dest,
  runner_on_1st_dest,
  runner_on_2nd_dest,
  runner_on_3rd_dest,
  outs_on_play)
SELECT 
  game_id,
  event_id,
  inning,
  batting_team,
  first_runner,
  second_runner,
  third_runner,
  outs,
  batter_event_flag,
  batter_dest,
  runner_on_1st_dest,
  runner_on_2nd_dest,
  runner_on_3rd_dest,
  outs_on_play
FROM retrosheet.events;

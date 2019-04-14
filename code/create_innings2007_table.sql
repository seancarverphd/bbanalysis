DROP TABLE IF EXISTS retrosheet.innings2007;
CREATE TABLE retrosheet.innings2007 (
  game_id VARCHAR(12) NOT NULL,
  inning TINYINT NOT NULL,                    -- 1 or higher
  batting_team BOOLEAN NOT NULL,              -- 0=visitor, 1=home
  home_team VARCHAR(3) DEFAULT NULL,
  visiting_team VARCHAR(3) DEFAULT NULL,
  at_bat VARCHAR(3) DEFAULT NULL,
  at_field VARCHAR(3) DEFAULT NULL,
  year INT(4) DEFAULT NULL,
  date DATE  DEFAULT NULL,
  double_header_game INT(1) DEFAULT NULL,
  finalxxx BOOLEAN DEFAULT NULL,
  all_batter BOOLEAN DEFAULT NULL,
  sequence VARCHAR(250) DEFAULT NULL,
  u_sequence DOUBLE DEFAULT NULL,
  PRIMARY KEY (game_id,inning,batting_team)
) ENGINE=MyISAM;

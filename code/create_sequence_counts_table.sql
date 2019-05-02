CREATE TABLE sequence_counts SELECT sequence, COUNT(*) FROM innings GROUP BY sequence;
ALTER TABLE sequence_counts CHANGE `COUNT(*)` count BIGINT;

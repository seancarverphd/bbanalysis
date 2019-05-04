USE retroinnings
DROP TABLE IF EXISTS sequence_counts
CREATE TABLE sequence_counts SELECT sequence, MAX(u_sequence), COUNT(*) FROM innings GROUP BY sequence;
ALTER TABLE sequence_counts CHANGE `COUNT(*)` count BIGINT;
ALTER TABLE sequence_counts ADD COLUMN relative_freq DOUBLE;
UPDATE sequence_counts SET relative_freq=count/2789488;
ALTER TABLE sequence_counts ADD COLUMN v_sequence DOUBLE;
UPDATE sequence_counts SET v_sequence=log10(2789488)-log10(count);


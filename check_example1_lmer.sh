#!

# compare latest Process*.trig file with example1.trig (for lmer)
# remove time-dependent IDs

ls -t semLMM/Process* | head -1 | xargs -Ifile cp file semLMM/tmp.trig
sed -e 's/1555[0-9]*/XXX/g' semLMM/tmp.trig > semLMM/latest.trig
diff semLMM/example1.trig semLMM/latest.trig
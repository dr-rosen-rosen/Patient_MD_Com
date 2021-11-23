##########################################################################
##########################################################################
################# LSM measure scripts
##########################################################################
##########################################################################

# To dos:
  # QC: Test whethere there are jsut two speaker roles per file; THis assumes we've already cleaned to just two roles somewhere... 
  # Add Vader scores to dataframe
  # Calculate matching for ALL LSM features
    # Groupby File, mutate_all (not sure about syntax), ungroup
    # This should give you a dataframe by FILE (not speaker) of LSM for each feature
  # Average over the groupings we care about:
    # Starndard LSM: ['ppron','ipron', 'article', 'prep', 'auxverb', 'adverb','negate','conj','quant']
    # mutate LSM = mean( of the LSM features)
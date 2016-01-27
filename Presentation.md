A Text Prediction Algorithm with a Web Interface
========================================================
author: Keith Miller
date: 19th January 2016


Building the model
========================================================

- Model trained on a cleaned corpus of around 17.7 million words randomly sourced from [twitter, blogs and internet newsfeeds](http://www.corpora.heliohost.org/aboutcorpus.html)
- Tagging of unknown words, sentence boundaries and digits was done to allow those contexts to be incorporated into the model
- The model uses ngrams with a maximum order of 4.  This was chosen as a good balance between model size and accuracy
- The training set contained around 70,000 unique words. In the model this was limited to the approximately 15,000 words which  account for 95% of word instances seen in the training corpus
- Available corpus was divided into 3 parts - the first to train the model, the second to set $\lambda_{1-4}\$ and the last to test accuracy and performance.
- The model was constructed using the *data table* package because of its superior performance with large table lookups


The Algorithm
========================================================

 - Several different algorithms were reviewed including simple bigram, trigram  and quadrigram, "Stupid Backoff", linear interpolated and linear interpolated with Good Turing Smoothing.  
 - Each was tested for accuracy using samples from the test set and the mean prediction time and accuracy calculated
 - Linear interpolation (see equation) with Good Turing smoothing of lower order ngram counts provided the best accuracy of 18% and a prediction time of less than 0.1 seconds
 
  $$
  \begin{aligned}
   P(w_i) = \lambda_1P(w_i) + \lambda_2P(w_i|w_{i-1}) +
   \lambda_3P(w_i|w_{i-2},w_{i-1}) + \\ \lambda_4P(w_i,|w_{i-3},w_{i-2},w_{i-1})
  \end{aligned}
  $$
  
  
 - Ngram weights ($\lambda_{1-4}$) were optimized using the held out set
 

 
The Algorithm - Good-Turing Smoothing
========================================================
 
The following discount calculation was made for each order of ngram to account for bigrams that have never been seen. It removes probability mass from less frequently seen ngrams and "gives" it unseen ngrams.  Its purpose is to make the prediction of less frequently seen ngrams less probable as they are not that much more frequently seen than unseen ngrams

This is the equation for smoothing bigrams counts of less than 5

$$
\begin{aligned}

P_{GT}(w_i|w_{i-1}) = \frac{\frac{(C(w_i)+1) N_{c+1}}{N_c}}{C(w_{i-1})}
\end{aligned}
$$
where $N_c$ is the number of bigrams with count C. 

Once the  smoothed probabilities were calculated, singleton ngrams were then discarded to reduce model size.

The Application
========================================================

 - To make the [app](https://keithdkm.shinyapps.io/Production/) easy to use, text prediction is automatically completed after the user presses the spacebar (or twice after a sentence ending)
 - Application also offers the three next most likely words which user can easily select by hitting one of three word selection buttons
 - Recognizes sentences boundaries and predicts with capitalized words
 - Recognizes digits and will predict accordingly
 - Actual performance against a held out test set is 17% accuracy with 1.5% standard deviation
 - How fast is it - average prediction time on i7 laptop with 8Gb RAM is 0.06secs
 

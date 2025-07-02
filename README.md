# Anna-Karenina-EDA-in-R
1. Word Frequency Analysis

   We analyze the frequency of words in the novel after cleaning and preprocessing the text, with a focus on identifying the top 20 most frequently used non-trivial words — excluding common stop words and the names of main characters.

    ![image](https://github.com/user-attachments/assets/bcfab52a-62d3-4b3b-a2d9-012ff7de7d95)

   Foundings:
   - Frequency analysis reveals a consistent presence of key themes such as "life", "love", "wife", "people", "husband" reflecting both everyday and philosophical aspects of the work.
   - By excluding character names and stop words, it is evident that Tolstoy uses a relatively focused but expressive vocabulary, especially when describing human emotions and states.

2. Character Mentions

   Narrative structure and character dynamics in Leo Tolstoy’s Anna Karenina using quantitative text analysis. By leveraging character name normalization, mention frequency tracking, and visual storytelling through heatmaps, we provide a data-driven perspective on the novel’s key figures and their prominence across the storyline.

   ![image](https://github.com/user-attachments/assets/4f84434c-bd19-4673-a124-ddb745147175)

   Foundings:
   - The frequency of mentions shows that the narrative centers around several main characters (Anna, Levin, Vronsky), whose storylines mostly do not directly intersect but alternate as the story progresses.
   - Visually, certain characters dominate specific parts of the book, confirming the presence of parallel narrative arcs.

3. Narrative Network Analysis

   Our goal is to explore how characters interact across the novel’s timeline and to uncover hidden narrative structures and relationships through computational literary analysis.

   ![image](https://github.com/user-attachments/assets/b076808a-0d02-49f5-a013-f59b444520d6)

   Foundings:
   - The co-mention network reveals the density of interactions among characters: Anna and Vronsky form a tight cluster, while Levin is more isolated but still connected.
   - Removing weak ties allows focus on the most significant relationships, highlighting the thematic structure of the novel — love drama, family conflicts, and philosophical reflections.
   

5. Latent Dirichlet Allocation (LDA)

   This visualization applies topic modeling to Anna Karenina by Leo Tolstoy using Latent Dirichlet Allocation (LDA) in R. The goal is to uncover hidden themes and semantic structures across the novel by analyzing recurring word patterns across segments of the text.

   ![image](https://github.com/user-attachments/assets/0006b37f-557b-4f6f-91a7-447ac11af78a)

   Foundings:
   - Topic modeling shows the novel can be divided into several hidden themes: love, religion, society, family, and death.
   - Despite the complexity of language, the themes form clearly, and the word distribution across topics aligns with the novel’s main plots.
   

7. Sentiment Analysis

   This visualization performs a rolling sentiment analysis of Anna Karenina using the Bing lexicon. The goal is to visualize the emotional dynamics of the novel by mapping how positive and negative sentiments fluctuate across the narrative.
   
   To observe sentiment trends over time, we calculated rolling means (moving averages) with two different window sizes:
   200 words: for local, fine-grained sentiment changes
   2000 words: for broader narrative trends

   ![image](https://github.com/user-attachments/assets/208f5f8e-d02a-4031-b5e8-91e9d6736d50)

   Foundings:
   - Rolling sentiment analysis reveals emotional waves: sharp declines correspond to crises or tragic events (e.g., death, conflict), while rises indicate moments of love, hope, or philosophical insight.
   - The smaller window (200 words) captures local emotional spikes, whereas the larger window (2000 words) reveals a smoother emotional curve reflecting the novel’s overall emotional rhythm.
   

9. Word Frequency Analysis for Main Characters

   These visualizations perform a contextual lexical analysis of the main characters in Anna Karenina, focusing on the words that surround each character throughout the text. The aim is to understand how language, themes, and emotional tone vary depending on the character — and thus gain insight into how Tolstoy constructs their psychological and narrative space. (Anna Karenina, Kitty Scherbatsky, Konstantin Levin, Alexey Vronsky)

   ![image](https://github.com/user-attachments/assets/ca90cf43-247e-4861-a037-277a92d41b9e)

   Near Anna: emotionally charged words like fear, love, eyes, night, reflecting her internal struggle.

   ![image](https://github.com/user-attachments/assets/19aee4bb-d115-4b17-951a-3f375ec3cab4)

   Near Kitty: family and social context words such as home, children, parents.

   ![image](https://github.com/user-attachments/assets/ce494ac2-513e-4196-9bce-a08cfcfc31f2)

   Near Levin: philosophical and everyday life themes like life, work, God, thought.

   ![image](https://github.com/user-attachments/assets/4018e64c-e867-4bd4-b817-aa3767548b37)

   Near Vronsky: action- and decision-related words like officer, anger, leave.



   








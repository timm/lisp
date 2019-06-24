;%
;% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;%
;% Weight treated as the class attribute. Identifier deleted.
;%
;% As used by Kilpatrick, D. & Cameron-Jones, M. (1998). Numeric prediction
;% using instance-based learning with encoding length selection. In Progress
;% in Connectionist-Based Information Systems. Singapore: Springer-Verlag.
;%
;% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;%
;% NAME:  fishcatch
;% TYPE:  Sample
;% SIZE:  159 observations, 8 variables
;%
;% DESCRIPTIVE ABSTRACT:
;%
;% 159 fishes of 7 species are caught and measured. Altogether there are
;% 8 variables.  All the fishes are caught from the same lake
;% (Laengelmavesi) near Tampere in Finland.
;%
;% SOURCES:
;% Brofeldt, Pekka: Bidrag till kaennedom on fiskbestondet i vaera
;%         sjoear. Laengelmaevesi. T.H.Jaervi: Finlands Fiskeriet  Band 4,
;%         Meddelanden utgivna av fiskerifoereningen i Finland.
;%         Helsingfors 1917
;%
;% VARIABLE DESCRIPTIONS:
;%
;% 1  Obs       Observation number ranges from 1 to 159
;% 2  Species   (Numeric)
;%         Code Finnish  Swedish    English        Latin
;%          1   Lahna    Braxen     Bream          Abramis brama
;%          2   Siika    Iiden      Whitewish      Leusiscus idus
;%          3   Saerki   Moerten    Roach          Leuciscus rutilus
;%          4   Parkki   Bjoerknan  ?              Abramis bjrkna
;%          5   Norssi   Norssen    Smelt          Osmerus eperlanus
;%          6   Hauki    Jaedda     Pike           Esox lucius
;%          7   Ahven    Abborre    Perch          Perca fluviatilis
;%
;% 3  Weight      Weight of the fish (in grams)
;% 4  Length1     Length from the nose to the beginning of the tail (in cm)
;% 5  Length2     Length from the nose to the notch of the tail (in cm)
;% 6  Length3     Length from the nose to the end of the tail (in cm)
;% 7  Height%     Maximal height as % of Length3
;% 8  Width%      Maximal width as % of Length3
;% 9  Sex         1 = male 0 = female
;%
;%
;%
;%           ___/////___                  _
;%          /           \    ___          |
;%        /\             \_ /  /          H
;%      <   )            __)  \           |
;%        \/_\\_________/   \__\          _
;%
;%      |------- L1 -------|
;%      |------- L2 ----------|
;%      |------- L3 ------------|
;%
;%
;% Values are aligned and delimited by blanks.
;% Missing values are denoted with NA.
;% There is one data line for each case.
;%
;% SPECIAL NOTES:
;% I have usually calculated
;%            Height =  Height%*Length3/100
;%            Widht  =  Widht%*Length3/100
;%
;%
;% PEDAGOGICAL NOTES:
;% I have mainly used only  Species=7 (Perch) and here is some of the
;% models and test, we have used
;%
;%       Weight=a+b*(Length3*Height*Width)+epsilon
;%          Ho: a=0;
;%          Heteroscedastic case. Question: What is proper weighting,
;%          if you use Length3 as a weighting variable.
;%
;%       Log(Weight)=a+b1*Length3+epsilon
;%
;%       Weight^(1/3)=a+b1*Length3+epsilon
;%       (Given by Box-Cox-transformation)
;%          Ho: a=0;
;%
;%       Log(Weight)=a+b1*Length3+b2*Height+b3*Width+epsilon
;%          Ho: b1+b2+b3=3;
;%          i.e. dimension of the fish = 3
;%
;%       Weight^(1/3)=a+b1*Length3+b2*Height+b3*Width+epsilon
;%       (Given by Box-Cox-transformation)
;%          Ho: a=0;
;%
;%       Weight=a*Length3^b1*Height^b2*Width^b3+epsilon
;%          Nonlinear, heteroscedastic case.
;%          What is proper weighting?
;%
;%       Is obs 143
;%
;%       143  7  840.0 32.5  35.0  37.3  30.8  20.9  0
;%
;%       an outlier? It had in its stomach 6 roach.
;%
;%
;%
;% REFERENCES:
;% Brofeldt, Pekka: Bidrag till kaennedom on fiskbestondet i vaara
;%         sjoear. Laengelmaevesi. T.H.Jaervi: Finlands Fiskeriet  Band 4,
;%         Meddelanden utgivna av fiskerifoereningen i Finland.
;%         Helsingfors 1917
;%
;%
;% SUBMITTED BY:
;% Juha Puranen
;% Departement of statistics
;% PL33 (Aleksanterinkatu 7)
;% 000014 University of Helsinki
;% Finland
;% e-mail: jpuranen@noppa.helsinki.fi
;%
(defun fishcatch ()
  (data
    :name 'fishcatch
    :columns '(Species Length1 Length2 Length3 Height Width Sex class)
    :egs
    '(
      (1 23.2 25.4 30 38.4 13.4 ? 242)
      (1 24 26.3 31.2 40 13.8 ? 290)
      (1 23.9 26.5 31.1 39.8 15.1 ? 340)
      (1 26.3 29 33.5 38 13.3 ? 363)
      (1 26.5 29 34 36.6 15.1 ? 430)
      (1 26.8 29.7 34.7 39.2 14.2 ? 450)
      (1 26.8 29.7 34.5 41.1 15.3 ? 500)
      (1 27.6 30 35 36.2 13.4 ? 390)
      (1 27.6 30 35.1 39.9 13.8 ? 450)
      (1 28.5 30.7 36.2 39.3 13.7 ? 500)
      (1 28.4 31 36.2 39.4 14.1 ? 475)
      (1 28.7 31 36.2 39.7 13.3 ? 500)
      (1 29.1 31.5 36.4 37.8 12 ? 500)
      (1 29.4 32 37.2 40.2 13.9 1 600)
      (1 29.4 32 37.2 41.5 15 ? 600)
      (1 30.4 33 38.3 38.8 13.8 1 700)
      (1 30.4 33 38.5 38.8 13.5 ? 700)
      (1 30.9 33.5 38.6 40.5 13.3 ? 610)
      (1 31 33.5 38.7 37.4 14.8 ? 650)
      (1 31.3 34 39.5 38.3 14.1 1 575)
      (1 31.4 34 39.2 40.8 13.7 ? 685)
      (1 31.5 34.5 39.7 39.1 13.3 ? 620)
      (1 31.8 35 40.6 38.1 15.1 ? 680)
      (1 31.9 35 40.5 40.1 13.8 ? 700)
      (1 31.8 35 40.9 40 14.8 1 725)
      (1 32 35 40.6 40.3 15 ? 720)
      (1 32.7 36 41.5 39.8 14.1 ? 714)
      (1 32.8 36 41.6 40.6 14.9 ? 850)
      (1 33.5 37 42.6 44.5 15.5 0 1000)
      (1 35 38.5 44.1 40.9 14.3 0 920)
      (1 35 38.5 44 41.1 14.3 ? 955)
      (1 36.2 39.5 45.3 41.4 14.9 1 925)
      (1 37.4 41 45.9 40.6 14.7 0 975)
      (1 38 41 46.5 37.9 13.7 ? 950)
      (2 23.6 26 28.7 29.2 14.8 ? 270)
      (2 24.1 26.5 29.3 27.8 14.5 ? 270)
      (2 25.6 28 30.8 28.5 15.2 ? 306)
      (2 28.5 31 34 31.6 19.3 ? 540)
      (2 33.7 36.4 39.6 29.7 16.6 0 800)
      (2 37.3 40 43.5 28.4 15 ? 1000)
      (3 12.9 14.1 16.2 25.6 14 ? 40)
      (3 16.5 18.2 20.3 26.1 13.9 ? 69)
      (3 17.5 18.8 21.2 26.3 13.7 ? 78)
      (3 18.2 19.8 22.2 25.3 14.3 ? 87)
      (3 18.6 20 22.2 28 16.1 ? 120)
      (3 19 20.5 22.8 28.4 14.7 ? 0)
      (3 19.1 20.8 23.1 26.7 14.7 0 110)
      (3 19.4 21 23.7 25.8 13.9 0 120)
      (3 20.4 22 24.7 23.5 15.2 0 150)
      (3 20.5 22 24.3 27.3 14.6 0 145)
      (3 20.5 22.5 25.3 27.8 15.1 0 160)
      (3 21 22.5 25 26.2 13.3 ? 140)
      (3 21.1 22.5 25 25.6 15.2 0 160)
      (3 22 24 27.2 27.7 14.1 ? 169)
      (3 22 23.4 26.7 25.9 13.6 ? 161)
      (3 22.1 23.5 26.8 27.6 15.4 0 200)
      (3 23.6 25.2 27.9 25.4 14 ? 180)
      (3 24 26 29.2 30.4 15.4 ? 290)
      (3 25 27 30.6 28 15.6 0 272)
      (3 29.5 31.7 35 27.1 15.3 ? 390)
      (4 13.5 14.7 16.5 41.5 14.1 ? 55)
      (4 14.3 15.5 17.4 37.8 13.3 1 60)
      (4 16.3 17.7 19.8 37.4 13.5 1 90)
      (4 17.5 19 21.3 39.4 13.7 1 120)
      (4 18.4 20 22.4 39.7 14.7 ? 150)
      (4 19 20.7 23.2 36.8 14.2 ? 140)
      (4 19 20.7 23.2 40.5 14.7 0 170)
      (4 19.8 21.5 24.1 40.4 13.1 0 145)
      (4 21.2 23 25.8 40.1 14.2 ? 200)
      (4 23 25 28 39.6 14.8 0 273)
      (4 24 26 29 39.2 14.6 0 300)
      (5 9.3 9.8 10.8 16.1 9.7 1 6.7)
      (5 10 10.5 11.6 17 10 0 7.5)
      (5 10.1 10.6 11.6 14.9 9.9 1 7)
      (5 10.4 11 12 18.3 11.5 0 9.7)
      (5 10.7 11.2 12.4 16.8 10.3 1 9.8)
      (5 10.8 11.3 12.6 15.7 10.2 1 8.7)
      (5 11.3 11.8 13.1 16.9 9.8 1 10)
      (5 11.3 11.8 13.1 16.9 8.9 0 9.9)
      (5 11.4 12 13.2 16.7 8.7 0 9.8)
      (5 11.5 12.2 13.4 15.6 10.4 0 12.2)
      (5 11.7 12.4 13.5 18 9.4 0 13.4)
      (5 12.1 13 13.8 16.5 9.1 0 12.2)
      (5 13.2 14.3 15.2 18.9 13.6 0 19.7)
      (5 13.8 15 16.2 18.1 11.6 0 19.9)
      (6 30 32.3 34.8 16 9.7 ? 200)
      (6 31.7 34 37.8 15.1 11 0 300)
      (6 32.7 35 38.8 15.3 11.3 ? 300)
      (6 34.8 37.3 39.8 15.8 10.1 ? 300)
      (6 35.5 38 40.5 18 11.3 ? 430)
      (6 36 38.5 41 15.6 9.7 1 345)
      (6 40 42.5 45.5 16 9.5 ? 456)
      (6 40 42.5 45.5 15 9.8 ? 510)
      (6 40.1 43 45.8 17 11.2 ? 540)
      (6 42 45 48 14.5 10.2 ? 500)
      (6 43.2 46 48.7 16 10 0 567)
      (6 44.8 48 51.2 15 10.5 0 770)
      (6 48.3 51.7 55.1 16.2 11.2 ? 950)
      (6 52 56 59.7 17.9 11.7 ? 1250)
      (6 56 60 64 15 9.6 ? 1600)
      (6 56 60 64 15 9.6 0 1550)
      (6 59 63.4 68 15.9 11 0 1650)
      (7 7.5 8.4 8.8 24 16 ? 5.9)
      (7 12.5 13.7 14.7 24 13.6 ? 32)
      (7 13.8 15 16 23.9 15.2 ? 40)
      (7 15 16.2 17.2 26.7 15.3 ? 51.5)
      (7 15.7 17.4 18.5 24.8 15.9 ? 70)
      (7 16.2 18 19.2 27.2 17.3 ? 100)
      (7 16.8 18.7 19.4 26.8 16.1 ? 78)
      (7 17.2 19 20.2 27.9 15.1 ? 80)
      (7 17.8 19.6 20.8 24.7 14.6 ? 85)
      (7 18.2 20 21 24.2 13.2 ? 85)
      (7 19 21 22.5 25.3 15.8 ? 110)
      (7 19 21 22.5 26.3 14.7 ? 115)
      (7 19 21 22.5 25.3 16.3 1 125)
      (7 19.3 21.3 22.8 28 15.5 0 130)
      (7 20 22 23.5 26 14.5 0 120)
      (7 20 22 23.5 24 15 ? 120)
      (7 20 22 23.5 26 15 ? 130)
      (7 20 22 23.5 25 15 ? 135)
      (7 20 22 23.5 23.5 17 0 110)
      (7 20.5 22.5 24 24.4 15.1 0 130)
      (7 20.5 22.5 24 28.3 15.1 0 150)
      (7 20.7 22.7 24.2 24.6 15 ? 145)
      (7 21 23 24.5 21.3 14.8 ? 150)
      (7 21.5 23.5 25 25.1 14.9 ? 170)
      (7 22 24 25.5 28.6 14.6 ? 225)
      (7 22 24 25.5 25 15 ? 145)
      (7 22.6 24.6 26.2 25.7 15.9 ? 188)
      (7 23 25 26.5 24.3 13.9 0 180)
      (7 23.5 25.6 27 24.3 15.7 ? 197)
      (7 25 26.5 28 25.6 14.8 ? 218)
      (7 25.2 27.3 28.7 29 17.9 0 300)
      (7 25.4 27.5 28.9 24.8 15 0 260)
      (7 25.4 27.5 28.9 24.4 15 ? 265)
      (7 25.4 27.5 28.9 25.2 15.8 0 250)
      (7 25.9 28 29.4 26.6 14.3 ? 250)
      (7 26.9 28.7 30.1 25.2 15.4 0 300)
      (7 27.8 30 31.6 24.1 15.1 0 320)
      (7 30.5 32.8 34 29.5 17.7 ? 514)
      (7 32 34.5 36.5 28.1 17.5 ? 556)
      (7 32.5 35 37.3 30.8 20.9 0 840)
      (7 34 36.5 39 27.9 17.6 0 685)
      (7 34 36 38.3 27.7 17.6 0 700)
      (7 34.5 37 39.4 27.5 15.9 0 700)
      (7 34.6 37 39.3 26.9 16.2 0 690)
      (7 36.5 39 41.4 26.9 18.1 0 900)
      (7 36.5 39 41.4 26.9 14.5 ? 650)
      (7 36.6 39 41.3 30.1 17.8 ? 820)
      (7 36.9 40 42.3 28.2 16.8 0 850)
      (7 37 40 42.5 27.6 17 0 900)
      (7 37 40 42.4 29.2 17.6 0 1015)
      (7 37.1 40 42.5 26.2 15.6 0 820)
      (7 39 42 44.6 28.7 15.4 0 1100)
      (7 39.8 43 45.2 26.4 16.1 0 1000)
      (7 40.1 43 45.5 27.5 16.3 0 1100)
      (7 40.2 43.5 46 27.4 17.7 1 1000)
      (7 41.1 44 46.6 26.8 16.3 0 1000)
      )))

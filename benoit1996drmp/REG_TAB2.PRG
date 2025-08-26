   /* --------------------------------------------------------------------
    |  This is the main file that analyzes the war data from             |
    |  Erich Weede's 1984 article "Democracy and War Involvment"         |
    |                                            JCR vol 28 no 4         |
    |                                                                    |
    |  This file replicates and extends Weede's Table 2 using regression |
    |  models.                                                           |
    |  It contains all of the war variables used in Weede, as well as    |
    |  all of the democracy scores analyzed in the paper,                |
    |  for the 101 nations which Weede analyzes in his paper.            |
    |                                                                    |
    |                                       --Ken Benoit                 |
    --------------------------------------------------------------------- */


/* **************************************
   *                                    *
   *      Initialization and misc       *
   *                                    *
   ************************************** */

library count;
filen = "weede";

call subdatv(filen,0);
v = getname(filen);

output file = reg_tab2.out reset;
countset;
__miss = 1;


/* ******************************************************
   *                                                    *
   *      I. ORDINARY LEAST SQUARES REGRESSION          *
   *                                                    *
   ****************************************************** */

proc(1) = normaliz(x);                 /* small proc to normalize a vector */
    local xnorm, x1;
    xnorm = (x-meanc(x))/stdc(x);
    retp(xnorm);
  endp;

proc(1) = removeMD(x);                 /* This was necessary b/c cant do   */
    local newx;                        /*     vector op's with MD           */
    newx = x[1:88] | x[90:rows(x)];    /* NOTE: this explicitly assumes that */
    retp(newx);                        /*  the MD item is Burma, row 89     */
  endp;


"                       normalized ---------------- ";
let __altnam = const poldem65 ssal680N;
call ols(0,normaliz(removeMD(ssal6080)),normaliz(removeMD(poldem65)));?;?;
let __altnam = const poldem65 kendeN;
call ols(0,normaliz(removeMD(kende)),normaliz(removeMD(poldem65)));?;?;
let __altnam = const poldem65 buttrwN;
call ols(0,normaliz(removeMD(butterw)),normaliz(removeMD(poldem65)));?;?;
?;" *** with Freedom House 1973";?;
let __altnam = const fh73 ssal680N;
call ols(0,normaliz(ssal6080),normaliz(fh73));?;?;
let __altnam = const fh73 kendeN;
call ols(0,normaliz(kende),normaliz(fh73));?;?;
let __altnam = const fh73 buttrwN;
call ols(0,normaliz(butterw),normaliz(fh73));?;?;
?;
"                       unnormalized -------------- ";
let __altnam = const poldem65 ssal6080;
call ols(0,ssal6080,poldem65);?;?;
let __altnam = const poldem65 kende;
call ols(0,kende,poldem65);?;?;
let __altnam = const poldem65 butterw;
call ols(0,butterw,poldem65);?;?;
?;" *** with Freedom House 1973";?;
let __altnam = const fh73 ssal6080;
call ols(0,ssal6080,fh73);?;?;
let __altnam = const fh73 kende;
call ols(0,kende,fh73);?;?;
let __altnam = const fh73 butterw;
call ols(0,butterw,fh73);?;?;
?;



/* ******************************************************
   *                                                    *
   *             II. POISSON REGRESSION                 *
   *                                                    *
   ****************************************************** */

/* I wrote the following proc because of the annoying way
   the POISSON proc only seems to work with a dataset file.
   because some of the data I want to analyze (like SSM_ALL
   and FH75_80 are transformed only in memory, this proc
   takes care of the problem by creating a temporary dataset
   in a file and analyzing that file */

proc(0) = do_poiss(dep,ind);
    __altnam = stof(ind);
    call countprt(poisson("weede",stof(dep),stof(ind)));
  endp;

/* --------------- 1960 - 1980 period -------------- */
let __altnam = const poldem65;
call do_poiss("ssm_is","poldem65");
call do_poiss("ssal6080","poldem65");
call do_poiss("kende","poldem65");


/* --------------- 1960 - 1974 period -------------- */
let __altnam = const poldem65;
call do_poiss("butterw","poldem65");
call do_poiss("ssis6074","poldem65");
call do_poiss("ssal6074","poldem65");

/* --------------- 1975 - 1980 period -------------- */
let __altnam = const fh75_80;
call do_poiss("ssis7580","poldem65");
call do_poiss("ssal7580","poldem65");

/* --------------- Additional  -------------- */
let __altnam = const fh73;
call do_poiss("ssal6080","fh73");
call do_poiss("butterw","fh73");



/* ******************************************************
   *                                                    *
   *        III. NEGATIVE BIONOMIAL REGRESSION          *
   *                                                    *
   *            (for  various  setups}                  *
   *                                                    *
   ****************************************************** */

countset;

proc(0) = nb(dep,ind);
    local i, d, b, bg, g, s2, vc, llik;
    _cnstart=0;
    @__altnam = ind;@
    {bg,vc,llik} = negbin("weede",stof(dep),stof(ind),0);
    g=bg[rows(bg)];
    s2=1+exp(g);
    call countprt(bg,vc,llik);
    "       sigma^2 =" s2;
    "DEP: ";; dep;; "    IND: ";; ind;?;?;
  endp;


?;?;
" ****************************************************************** ";
" *         Regression models to Replicate Weede's Table 2         * ";
" ****************************************************************** ";?;?;


/* Kende wars are for 1960-1980 */

call nb("kende","poldem65");
call nb("kende","fh73");


/* Singer-Small (all) wars from 1960-1980 */

call nb("ssal6080","poldem65");
call nb("ssal6080","fh73");

/* Butterworth wars from 1960-1974 */

call nb("butterw","poldem65");
call nb("butterw","fh73");

output file = off;


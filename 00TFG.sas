/****************************/
/****************************/
/****************************/
/*   SERIE DIARIA EUR/USD   */
/****************************/
/****************************/
/****************************/

libname st "C:\Users\HP\OneDrive\Escritorio\UNIVERSIDAD\TFG";

proc import file="C:\Users\HP\OneDrive\Escritorio\UNIVERSIDAD\TFG\eurusdclose.csv"
    out=st.EURUSDc
    dbms=csv
	replace;
	delimiter=";";
run;

/***********************************************************/
/* Se reservan los dos últimos años de datos para hacer la */
/* predicción y comprobar la capacidad de acierto          */
/***********************************************************/

data work.EURUSDc;
  set st.EURUSDc;
  log_Close=log(Close);
  recuerdaClose=Close;
  recuerdaLogClose=log_Close;
  if Date>'31dec2020'd then do;
    Close=.;
    log_Close=.;
  end;
run;

goptions device=activex;
ods html style=sasweb;

  proc gplot data=st.EURUSDc;
    plot Close*Date;
    symbol i=join v=dot h=0.1 c=black;
  run;
  quit;

  ods html close;

/********************************************************/
/* Se contrasta si la serie es estacionaria en varianza */
/********************************************************/
%boxcoxar(work.eurusdc,
          Close,
          nlambda=3,
          lambdalo=0,
          lambdahi=1);

/*************************************************************/
/* De acuerdo al AIC y SBC es preferible tomar logaritmos    */
/* Pues el AIC más bajo corresponde con el valor de lambda 0 */
/*************************************************************/

/*****************************************************/
/* Se contrasta si la serie es estacionaria en media */
/*****************************************************/

%dftest(work.eurusdc,
        log_Close,
        dlag=1);
%put &dfpvalue;

/* 0.432285204>0.05 -> Aparentemente se recomienda hacer una diferencia regular */

/************************************************************************************/
/* Se tomará la decisión de diferenciar sobre la marcha, ajustando el modelo SARIMA */
/************************************************************************************/

/***************************************************************/
/* Contrucción del periodograma para detectar estacionalidades */
/***************************************************************/

proc spectra data=ST.eurusdc center out=work.periodograma;
  var Close;
  where Date<='31dec2020'd;
run;
quit;

proc sort data=work.periodograma(where=(period<=31));
  by period;
run;
quit;

options device=activex;

proc gplot data=work.periodograma;
  plot p_01*period;
  symbol i=join v=dot h=0.5 c=blue;
run;
quit;

/***********************************************************************/
/* Aparentemente parece que no va a haber ninguna estacionalidad clara */
/*	  Por lo que probablemente nuestro modelo será un ARIMA(p,d,q)     */
/***********************************************************************/

/* Tomamos directamente la serie de los retornos */

ods html;
proc arima data=work.EURUSDc;
  identify var=log_Close;
run;
quit;

proc arima data=work.EURUSDc;
  identify var=log_Close;
  estimate plot p=1 noint;
run;
quit;

proc arima data=work.EURUSDc;
  identify var=log_Close(1);
  estimate plot noint;
  forecast out=work.salidaARIMA id=Date;
run;
quit;

/* Una mierda, pero sabíamos que iba a pasar */

/************************************************************/
/* Macro para realizar el test de normalidad de Jarque-Bera */
/************************************************************/

%macro jbtest(data = , var = );

   ods listing close;
   proc univariate data = &data ;
       var &var;
      ods output moments = _1;
   run;
  
   data _2;
      set _1;
      label = label1; value = nValue1; output;
      label = label2; value = nValue2; output;
      drop cvalue: label1 label2 nvalue:;
   run;

   proc transpose data = _2 out = _3;
      by varname notsorted;
      id label;
      var value;
   run;

   data _4;
      set _3;
      jb = (asimetr_a**2 * n)/6 + ((curtosis)**2 *n)/24;
      p = 1 - probchi(jb, 2);
      lable jb = 'JB Statistic' p = 'P-value'
         curtosis = 'Excess Kurtosis';
   run;
   ods listing;
   proc print data = _4 label;
      var varname n asimetr_a curtosis jb p;
   run;
/*
   proc datasets nolist;
      delete _:;
   quit;
*/
%mend;

/* Run a test using a data set from SAS's HELP library */
%jbtest(data = WORK.salidaARIMA,var = residual); 


/*************************************/
/* Los residuos no son normales      */
/* y por tanto no son independientes */
/* pudiendo haber estructura ARCH    */
/*************************************/

/************************************/
/* Análisis de asimetría y curtosis */
/************************************/

proc univariate data=work.salidaARIMA normal plot;
  var residual;
  histogram/kernel(color=red w=1) cfill=blue;
run;
quit;

/******************************************/
/* Análisis de los residuales cuadráticos */
/******************************************/

data work.salidaARIMA;
  set work.salidaARIMA;
  residualCuadrado=residual**2;
run;

goptions device=activex;
ods html style=sasweb;

proc gplot data=salidaARIMA;
  plot residualCuadrado*Date/haxis=('27dec1979'd to '01jan2021'd by 365);
  symbol i=join v=dot h=0.1 c=blue;
run;
quit;

ods html close;

ods html style=sasweb;
ods graphics on;

proc arima data=work.salidaARIMA;
  identify var=residualCuadrado;
run;
quit;

proc autoreg data=work.salidaARIMA;
  model residual=/archtest;
run;
quit;

proc autoreg data=work.salidaARIMA;
  model residual=/garch=(q=1,p=1);
run;
quit;

proc arima data=work.salidaARIMA;
  identify var=residualCuadrado;
  estimate p=1 q=1;
run;
quit;

proc arima data=work.salidaARIMA;
  identify var=residualCuadrado(1);
  estimate q=1;
run;
quit;

proc autoreg data=work.salidaARIMA;
  model residual=/garch=(q=1,p=1, type=igarch);
run;
quit;


/******************/
/******************/
/* GARCH en MEDIA */
/******************/
/******************/

proc autoreg data=work.salidaARIMA;
  model residual=/garch=(q=1,p=1,mean=LOG) noint;
  output out=work.salidaAUTOREG 
  cev=predVarianzaCondicional;
run;
quit;

/*******************/
/*******************/
/* GARCH NO LINEAL */
/*******************/
/*******************/

/*************************************/
/* Análisis de efecto apalancamiento */
/*************************************/

data work.salidaARIMA;
  set work.salidaARIMA;
  residual1=lag(residual);
  residual2=lag2(residual);
  residual3=lag3(residual);
  residual4=lag4(residual);
  residual5=lag5(residual);
  residual6=lag6(residual);
run;

proc reg data=work.salidaARIMA;
  model residualCuadrado=residual1 
                         residual2 
                         residual3 
                         residual4 
                         residual5 
                         residual6;
run;
quit;

/***************************************************/
/* Ajustamos un GARCH(1,1) exponencial (no lineal) */
/***************************************************/

proc autoreg data=work.salidaARIMA;
  model residual=/garch=(q=1,p=1,type=exp) noint;
  output out=work.salidaAUTOREG 
  cev=predVarianzaCondicional;
run;
quit;

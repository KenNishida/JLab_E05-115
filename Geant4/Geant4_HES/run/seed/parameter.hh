
  double me=0.000511;
  double mk=0.493667;
  double mp=0.938272;
  double amu=0.931494;
  double mC12=11.17486;
  double mB11=10.25255;
  double mCr52=48.37000;
  double mV51=47.44223;
  double ml=1.115683;
  double ms0=1.192642;
  double Bl_B12gs=0.01137;
  double Bl_B12ce1=0.009;
  double Bl_B12ce2=0.005;
  double Bl_B12ps=0.0;
  double Bl_V52gs=0.020;
  double mhy_B12gs=mB11+ml-Bl_B12gs;
  double mhy_B12ce1=mB11+ml-Bl_B12ce1;
  double mhy_B12ce2=mB11+ml-Bl_B12ce2;
  double mhy_B12ps=mB11+ml-Bl_B12ps;
  double mhy_V52gs=mV51+ml-Bl_V52gs;
  double alpha=1./137.;
  double pi=3.1415926;
  double echarge=1.6*1e-19;
  double AvoNum=6.02*1e23;
  double BeamCurrent=30.*1e-6;
  double TargetThick=100.;
  double absCS=1e-31;
  
  int event=0,accevent=0,flag=0,accflag=0,allevent=0,massflag=0;
  //pi is incoming electron mom and ps is scattered central elecrton mom
  double xMax=0.,yMax=0.,thMaxe=0.,zMax=0.,dpMax=0.,dpoff=0.,ypblk=0.;
  //x{y,z}Max is target size, thMax(dpMax) is the maximum of theta(dp) 
  double xpel=0.,xpeu=0.;
  double ypel=0.,ypeu=0.;
  double pkl=0.,pku=0.;
  double thMaxk=0.;
  double xpkl=0.,xpku=0.;
  double ypkl=0.,ypku=0.;
  int loop=0,tmass=0;
  double probRnd = 0.,prob=0.;
  double accepratio=0.;
  double IntGamma=0.;
  double maxRnd = 1.0;
  
  double Pin=0.,Ps=0.,Ei=0.;
  double xt=0.,yt=0.,zt=0.;
  
  double xpe=0.,ype=0.;
  double thetaie = 0.,phie = 0.;
  double dpe=0.,pe=0.;
  double pxe=0.,pye=0.,pze=0.;
  double Ee=0.;
  
  double xpk=0.,ypk=0.;
  double thetaik = 0.,phik = 0.;
  double dpk=0.,pscatk=0.;
  double pxk=0.,pyk=0.,pzk=0.;
  double pk=0.,Ek=0.;
  
  double xpg=0.,ypg=0.;
  double thetaig = 0.,phiig = 0.;
  double dpg=0.,pscatg=0.;
  double pxg=0.,pyg=0.,pzg=0.;
  double pg=0.,omega=0.;

  double mtar=0., mhy=0.;

  double thetaek=0.;
  double thetagk=0.;
  double phigk=0.;
  double thetaeg=0.;

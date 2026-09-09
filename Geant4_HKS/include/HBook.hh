/*
  HBook.h
*/

#ifndef HBook_h
#define HBook_h 1

#include <map>
#include <string>

class VHistogram
{
protected:
  std::string Title;
  int  Id;
  bool fEffective;
public:
  VHistogram();
  VHistogram( const std::string & title, int hid );
  virtual ~VHistogram() {}

  const std::string & title( void ) const { return Title; }
  int id( void )       const { return Id; }

  virtual void Fill1( double valx, double weight=1.0 ) = 0;
  virtual void Fill2( double valx, double valy, double weight=1.0 ) = 0;

  void Fill1( int valx, int weight )
  { Fill1( double(valx), double(weight) ); }

  void Fill1( int valx, double weight )
  { Fill1( double(valx), weight ); }

  void Fill1( int valx )
  { Fill1( double(valx) ); }

  void Fill2( int valx, int valy, int weight )
  { Fill2( double(valx), double(valy), double(weight) ); }

  void Fill2( int valx, int valy, double weight )
  { Fill2( double(valx), double(valy), weight ); }

  void Fill2( int valx, int valy )
  { Fill2( double(valx), double(valy), 1. ); }

  
  void Fill2( int valx, double valy, double weight )
  { Fill2( double(valx), valy, weight ); }

  void Fill2( int valx, double valy, int weight )
  { Fill2( double(valx), valy, double(weight) ); }

  void Fill2( int valx, double valy )
  { Fill2( double(valx), valy, 1.  ); }

  void Fill2( double valx, int valy, double weight ) 
  { Fill2( valx, double(valy), weight ); }

  void Fill2( double valx, int valy, int weight ) 
  { Fill2( valx, double(valy), double(weight) ); }

  void Fill2( double valx, int valy ) 
  { Fill2( valx, double(valy), 1. ); }

    
  virtual bool Register( void ) = 0;
  virtual void PrintTitle( void ) = 0;
  virtual void Reset( void ) = 0;
};

class Histogram1D : public VHistogram
{
private:
  int NBinX;
  double LowX, HighX;
public:
  Histogram1D();
  Histogram1D( const std::string & title, int hid, int nbinx, 
	       double lowx, double highx );
  ~Histogram1D() {}
private:
  Histogram1D( const Histogram1D & );
  Histogram1D & operator = ( const Histogram1D & );

public:  
  void Fill1( double valx, double weight=1. );
  void Fill2( double valx, double valy, double weight=1. );
  virtual bool Register( void );
  virtual void PrintTitle( void );
  virtual void Reset( void );
};

class Histogram2D : public VHistogram
{
private:
  int NBinX, NBinY;
  double LowX, HighX, LowY, HighY;
public:
  Histogram2D();
  Histogram2D( const std::string & title, int hid, int nbinx, double lowx,
	       double highx, int nbiny, double lowx, double highx );
  ~Histogram2D() {}
private:
  Histogram2D( const Histogram2D & );
  Histogram2D & operator = ( const Histogram2D & );

public:
  void Fill1( double valx, double weight=1. );
  void Fill2( double valx, double valy, double weight=1. );
  virtual bool Register( void );
  virtual void PrintTitle( void );
  virtual void Reset( void );
};


class HBook
{
 private:
  std::string HistoFileName;
  static HBook *fHBook;
 public:
  HBook();
  HBook( const std::string & filename );
  ~HBook();
private:
  HBook( const HBook & );
  HBook & operator = ( const HBook & );

public:
  static HBook *GetHBook( void ) { return fHBook; }
  void SetFileName( const std::string & filename ) { HistoFileName=filename; } 

  bool Store( void );
  bool Store( const std::string & filename );

private:
  typedef std::map <std::string, VHistogram *> TitleHolder;
  typedef std::map <int, VHistogram *> HidHolder;

  typedef std::pair <std::string, VHistogram *> TitlePair;
  typedef std::pair <int,    VHistogram *> HidPair;

  typedef std::map <std::string, VHistogram *>::const_iterator TitleIterator;
  typedef std::map <int,    VHistogram *>::const_iterator HidIterator;

  Histogram1D *defaultHistogram1D;
  Histogram2D *defaultHistogram2D;

  TitleHolder *TitleMap;
  HidHolder   *HidMap;

public:
  Histogram1D * Add1D( Histogram1D *histo );
  Histogram1D * Add1D( const std::string & title, int hid, int nbinx, 
		       double lowx, double highx );
  Histogram1D * Add1D( const char  *title, int hid, int nbinx, double lowx,
		       double highx );
  Histogram2D * Add2D( Histogram2D * histo );
  Histogram2D * Add2D( const std::string & title, int hid, int nbinx,
		       double lowx, double highx, int nbiny, 
		       double lowx, double highx );
  Histogram2D * Add2D( const char *title, int hid, int nbinx, double lowx,
		       double highx, int nbiny, double lowx, double highx );
  Histogram1D * Get1D( const std::string & title);
  Histogram1D * Get1D( const char * title );
  Histogram1D * Get1D( int hid );
  Histogram2D * Get2D( const std::string & title );
  Histogram2D * Get2D( const char * title );
  Histogram2D * Get2D( int hid );

  bool RegisterAll( void );
  void PrintTitleAll( void );
  void ResetAll( void ); 

private:
  VHistogram *GetH( const std::string & title );
  VHistogram *GetH( int hid );

};  

#endif

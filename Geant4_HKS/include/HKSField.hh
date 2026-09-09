/*
  HKSField.hh

  D.Kawama
*/

#ifndef HKSField_h

#define HKSField_h 1

//#include "G4ElectroMagneticField.hh"
#include "G4MagneticField.hh"
#include "globals.hh"


#include "G4ThreeVector.hh"
#include <vector>

class BeamLineElement;

// class G4ChordFinder;
// class G4EqMagElectricField;
// class G4MagIntegratorStepper;

//class HKSField : public G4ElectroMagneticField
class HKSField : public G4MagneticField
{
public:
  HKSField();
  //  explicit HKSField( const G4String &fileName );
  ~HKSField();
private:
  HKSField( const HKSField & );
  HKSField & operator = ( const HKSField & );

public:
  void GetFieldValue(const double Point[4], double* Bfield) const;

  void cleanupElemList( void );
  void AddElement( BeamLineElement *elem );

  void ResetQueue( void ) const;
  void PushInQueue( int val, const G4ThreeVector &pos ) const;
  void PrintQueue( void ) const;

private:
  typedef std::vector <BeamLineElement *> BLEContainer;
  typedef std::vector <BeamLineElement *>::const_iterator BLEIterator;

  BLEContainer elemList;

//   G4ChordFinder *fChordFinder;
//   G4EqMagElectricField *fEquation;
//   G4MagIntegratorStepper *fStepper;

  void MakeStepper( void );
  void DeleteStepper( void );

  mutable std::vector<int> fFlag;
  mutable std::vector<double> fPointX, fPointY, fPointZ;

};

#endif

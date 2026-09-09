/*
  HKSSteppingAction.hh

  H.Nomura
  2003/Dec/3
*/

#ifndef HKSSteppingAction_h
#define HKSSteppingAction_h 1

#include "G4UserSteppingAction.hh"

class G4Step;
class HKSField;

class HKSSteppingAction : public G4UserSteppingAction
{
public:
  HKSSteppingAction();
  ~HKSSteppingAction();

  void UserSteppingAction( const G4Step *aStep );

private:
  const HKSField *field_;

};
#endif
 

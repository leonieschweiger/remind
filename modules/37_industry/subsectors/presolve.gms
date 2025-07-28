*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/37_industry/subsectors/presolve.gms


*' Shares of SE/FE combinations among all chemicals carbonaceous fuels
*' is needed to split feedstocks carbon into SE and FE
*' We use vm_demFeNonEnergySector instead of vm_demFeSector_afterTax
*' since it is already restricted to chemicals carbonaceous fuels,
*' and equals vm_demFeSector_afterTax multiplied by a scalar factor,
*' which cancels out in the division.
p37_carbonaceousSeFeShare(t,regi,entySe,entyFe)$(
                         sum(te, se2fe(entySe,entyFe,te))
                     AND entyFE2sector2emiMkt_NonEn(entyFe,"indst","ETS") )
= vm_demFeNonEnergySector.l(t,regi,entySe,entyFe,"indst","ETS")
  / max(sm_eps,
    sum(se2fe(entySe2,entyFe2,te),
      vm_demFeNonEnergySector.l(t,regi,entySe2,entyFe2,"indst","ETS")))
;

*** EOF ./modules/37_industry/subsectors/presolve.gms

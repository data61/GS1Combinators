module Data.GS1.Objects where


data Object = PhysicalObject | DigitalObject
-- Phyiscal objects include trade items (products), logistic units, returnable assets, fixed assets, physical documents, etc
-- digital objects include digital trade items (musical items, digital books, etc), digital documents (electronic coupons, etc)
--

data ObjectID = ClassLevelID | InstanceLevelID

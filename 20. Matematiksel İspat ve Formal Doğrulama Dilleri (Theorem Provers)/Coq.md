# Coq

## Özet
Coq (Latince 'Horoz', ya da Kurucusu Thierry Coquand'ın Soyadı); 1989 Inria (Fransa) Enstitütüsünde Icad edilen, Yazılan Yazılımın (Kodun) İçinde "Gram Hata(Bug) Yoktur ve Bu Kod Asla Çökemez!" cümlesini, Basit Testler YAZARAK DEĞİL, Akademik Formüllerle **"MATEMATİKSEL OLARAK KANITLAYAN (Formal Verification / Theorem Proving)"**, Kariyer Zirvesi olan Devasa Bir İspat ve Mantık-Programlama Dilidir (Gallina dilini kullanır).

## Nedir ve Ne İşe Yarar?
Eğer bir "E-Ticaret Sitesi" Yazıyorsanız, Alışveriş sepeti 4-5 kez çöker, Adamlar Kodu düzeltir Hata testini Yapar(Unit Test) ve Yoluna Devam eder. Zira Ölümcül Değildir.
AMA! Eğer siz bir **Uçağın Otopoiliot Sistemini, Nükleer Reaktöre Su Pompalayan Çipi VEYAA Fransiz Metrosunun Sinyalizayon Tren Kodunu (Otomatik Süren)** Yazıyorsanız... Kodu 1 Milyar Kere de Test etseniz; Kafanızda "Acaba Benim Test Etmeyi Akıl Edemediğiğim 1 Milyar Birinci İhtimalde (Kuş Çarparsa Rüzgar gelirse vs) Tren İki Trene Çarpar Mı?" Diye Uyuyamazsınız. Can Güvenliği ve Milyar Dollakar Söz konusu İse TEST Etmek YETMEZ! "İspat Etmek" Gereekir.

Coq Dediki: Bana O Yazdığın Kodun Özelliklernii Gireksin. Ben Gece Boyunca Milyonlarca Matematiksel Calculus(Tümevarım) Kuralı Kasarak O Kodun İçerisiknidek Herhtangi BiR Uç ihtimali(Edge Case) Ezip GEçeceğim. Eğer Benim Elimden Sağ Çıkarsa; O kod MATEMATİKSEL OLARAK TEOREMDIR VE ASLA HATAAAA YAPAAAAMAAZ!

**Ne İşe Yarar?**
* **CompCert (C Derleyecisi):** Kendi kodunuz C++ olsun, Harikadır. Lakin Kodunuzu Kendi Kodunuzu Derleyen Motor (C Compiler - GCC) inİçinde Cok Milyarda Birde olsa BUG (Hata) Varsa Siz Çöktünüz. İşte Fransızlar **CompCert** Adından Bir C Derleyicisi Yapttılar ve İçinde Bug Olması "Matematiksel Olarak (Coq ile)" Imkansızlaştıroilmis En PAhali C Derleyecisidir. Havacaylıkta kullanır.
* Akıllı Kontratlar (Tezos Kripto Ağları): Bir finans Hvuunda Milyar dolarlar Tutuluyorsa Test Edilemez, Güvenlik Zİrvcesinde İSPAT (Coq ile) edilmek Zorunaddırlar. 

## Dilin Mantığı ve Kod Yapısı
Çook ağır bir dildir. Tip teorisi (Calculus of Inductive Constructions) üstüne inşa edliir. İki Aşaması vardır:
1. **Teorem Beyanı (Theorem/Lemma):** "Bu Yazılam İkitane Çİft SAyıyı Toplarsa Cevap Her Zaman Çift(Even) Çıkartır".
2. **Kanıt Taktikleri (Proof Tactics):** Bu cümlenin Doğruluğunu Felsefesi / Cebirse olarak İnce İnce İşlersiniz (`intros`, `induction`, `apply` Komutlarıyla).
   
Derleyici Tattiklerdeki Akışı Oku, Eğer O Teorem 1 Adet Bile zedeleniyorsa (Error Firlatir) "Kardeş Senin Kodunda Çatlak WAER!!" Der.

### Örnek Bir Coq Kodu: Bir + Bir'in İki Olduğunu, Veya Herhangi İki Sayının Toplamasının (A+B = B+A) Yer Değiştirebilcegini İSPATLAMA Macerası:
Bir Yazılımcı Coq ta (a+b = b+a) İse Bunu Testlerle "abi 5 ile 3 topladım 8 etti Cok guclu" YAMAZ!! Bunu Tum Evren İcin İspatlar. (Tümevarım Taktklerii):

```coq
(* BU BIR COQ MATH(THEOREM) ISPAT KODUDUR *)

(* Oncelikle Doğal SAyılar (Nat) kutuphanesini (ArkaPlan Matetmatigini) Cekelim *)
Require Import Arith.

(* 1. TEOREMİ DÜNYAYA ILAN ET! *)
(* Theorem : Toplama Islemi Degismezdir(Commutative).
   Eger 'n' ve 'm' diye Evrenden Iki Tane DoğalSAyı (nat) Verilrse, 
   (n + m) DAIMA VE KATİYYEN (m + n) ye Esittiriiir!!!! (Bunu Cözucez)
*)
Theorem toplama_degismezdir : forall n m : nat, n + m = m + n.


(* 2. İSPATA (PROOF) KARANLIK ODAYA GİR VE ÇARPIŞ !! *)
Proof.
  (* (intros): Disaridan Verilen 'n' ve 'm' Degiskenlernii Benim Felsefe Masama Getir! (Sahneye sok) *)
  intros n m.
  
  (* (induction) Tümevarım Taktiğini UYgula!! 'n' uzerinden Gitcesksin. 
     (Yani n yi once 0 Alıcaksn . Dogru cikarsa. n yi n+1(Sulcessor) alicksn Yine Deneiuycesim! *)
  induction n as [| n' IHn'].
  
  - (* Birinci Duum! : (Asil Durum- Base Case). Diyelkimki N =Sııfir (0).
       Sahne suna Donusur: 0 + m = m + 0. Bunun Dogulrugnu İsparla! *)
    
    (* Coq'un icindeki Dahili Basitelstiri() Simpl. Funcsyionuna sokarız Ve 
       Kendi İcindeki (plus_n_O) yani (Biir Sayiya 0Eklersne Kendisifider ) kuraluniuyulara Cizer! *)
    simpl. rewrite <- plus_n_O. reflexivity.
    
  - (* İkinnci DOruMm (Adimlama - Inductive Step). Diyelimki n degilde (n+1) gelseydo. 
       Mantik İcinde Su Formulu Uygula diyoruz !*)
    
    simpl. rewrite -> IHn'. rewrite -> plus_n_Sm. reflexivity.


(* 3. ALKIŞ VE KIYAMET !!! İSPAT BAŞARI İLE COZUMÜMLENMDİ (Quod Erat Demonstrandum - Q.E.D) *)
Qed.
```
Yukrakadaki bu `Qed` (Bitti!) Komutu Geldiğinde Coq Bilgisayarı "Evet! Gerçekternde A+B = B+A İspatlsnmsitir" Der ve Altına EFSANEVİ Yeil Çekigini ATAR. Siz Coq Terminalinin İçine Bakarkeen "Tanrı İle Sohbert eden (Matemtatigin Sırrını cizen)" Bir Filozof GİbisniziDirir. Acı verir, ama Nükeer Reaktöleri Patlamaktan kurtaran O "Kusursuz Yazılım(Zero-Bug)" Büyüsüdür.

## Kimler Kullanır?
* Evrendeki sadece **En Elit, Akademik Felsefe Kökenli, Formal Doğrulama (Formal Verification) Mühendisleri**. Silikon vadisinde Uçak (Boeing), Uzay (Nasa Mekiği Füzeleri), Nükleer ve İnsan Hayatına kast Edecek Otomasyon Cihazlarının Yazılımlarındaki C/Assembly Olarak Çıkan Kodu Coq Motoruna Sokarak Gece Başında Bekleyen İnsanlardır. (Bu adamlaara Saygiyla Egilir, Selam verrisliz). Dünyanın Programlama Ustasidirladr.

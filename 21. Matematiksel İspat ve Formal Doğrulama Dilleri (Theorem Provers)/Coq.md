# Coq

## Özet
Coq (Latince 'Horoz', ya da Kurucusu Thierry Coquand'ın Soyadı); 1989 Inria (Fransa) Enstitüsü'nde icat edilen, Yazılan Yazılımın (Kodun) İçinde "Gram Hata (Bug) Yoktur ve Bu Kod Asla Çökemez!" cümlesini, Basit Testler YAZARAK DEĞİL, Akademik Formüllerle **"MATEMATİKSEL OLARAK KANITLAYAN (Formal Verification / Theorem Proving)"**, Kariyer Zirvesi olan Devasa Bir İspat ve Mantık-Programlama Dilidir (Gallina dilini kullanır).

## Nedir ve Ne İşe Yarar?
Eğer bir "E-Ticaret Sitesi" Yazıyorsanız, Alışveriş sepeti 4-5 kez çöker, Adamlar Kodu düzeltir Hata testini Yapar(Unit Test) ve Yoluna Devam eder. Zira Ölümcül Değildir.
AMA! Eğer siz bir **Uçağın Otopilot Sistemini, Nükleer Reaktöre Su Pompalayan Çipi VEYA Fransız Metrosunun Sinyalizasyon Tren Kodunu (Otomatik Süren)** Yazıyorsanız... Kodu 1 Milyar Kere de Test etseniz; Kafanızda "Acaba Benim Test Etmeyi Akıl Edemediğim 1 Milyar Birinci İhtimalde (Kuş Çarparsa Rüzgar gelirse vs) Tren İki Trene Çarpar Mı?" Diye Uyuyamazsınız. Can Güvenliği ve Milyar Dolarlar Söz konusu İse TEST Etmek YETMEZ! "İspat Etmek" Gerekir.

Coq Dediki: Bana O Yazdığın Kodun Özelliklerini Gireceksin. Ben Gece Boyunca Milyonlarca Matematiksel Calculus(Tümevarım) Kuralı Kasarak O Kodun İçerisindeki Herhangi BiR Uç ihtimali(Edge Case) Ezip Geçeceğim. Eğer Benim Elimden Sağ Çıkarsa; O kod MATEMATİKSEL OLARAK TEOREMDİR VE ASLA HATAAAA YAPAAAAMAAZ!

**Ne İşe Yarar?**
* **CompCert (C Derleyicisi):** Kendi kodunuz C++ olsun, Harikadır. Lakin Kodunuzu Derleyen Motor (C Compiler - GCC) içinde Çok Milyarda Birde olsa BUG (Hata) Varsa Siz Çöktünüz. İşte Fransızlar **CompCert** Adında Bir C Derleyicisi Yaptılar ve İçinde Bug Olması "Matematiksel Olarak (Coq ile)" Imkansızlaştırılmış En Pahalı C Derleyicisidir. Havacılıkta kullanılır.
* Akıllı Kontratlar (Tezos Kripto Ağları): Bir finans havuzunda Milyar dolarlar Tutuluyorsa Test Edilemez, Güvenlik Zirvesinde İSPAT (Coq ile) edilmek Zorundadırlar. 

## Dilin Mantığı ve Kod Yapısı
Çook ağır bir dildir. Tip teorisi (Calculus of Inductive Constructions) üstüne inşa edliir. İki Aşaması vardır:
1. **Teorem Beyanı (Theorem/Lemma):** "Bu Yazılam İkitane Çİft SAyıyı Toplarsa Cevap Her Zaman Çift(Even) Çıkartır".
2. **Kanıt Taktikleri (Proof Tactics):** Bu cümlenin Doğruluğunu Felsefesi / Cebirse olarak İnce İnce İşlersiniz (`intros`, `induction`, `apply` Komutlarıyla).
   
Derleyici Taktiklerdeki Akışı Oku, Eğer O Teorem 1 Adet Bile zedeleniyorsa (Error Firlatır) "Kardeş Senin Kodunda Çatlak VAR!!" Der.

### Örnek Bir Coq Kodu: Bir + Bir'in İki Olduğunu, Veya Herhangi İki Sayının Toplanmasının (A+B = B+A) Yer Değiştirebileceğini İSPATLAMA Macerası:
Bir Yazılımcı Coq'ta (a+b = b+a) İse Bunu Testlerle "abi 5 ile 3 topladım 8 etti Çok güçlü" YAZMAZ!! Bunu Tüm Evren İçin İspatlar. (Tümevarım Taktikleri):

```coq
(* BU BIR COQ MATH(THEOREM) ISPAT KODUDUR *)

(* Oncelikle Doğal Sayılar (Nat) kütüphanesini (ArkaPlan Matematiğini) Cekelim *)
Require Import Arith.

(* 1. TEOREMİ DÜNYAYA İLAN ET! *)
(* Theorem : Toplama İşlemi Değişmezdir (Commutative).
   Eğer 'n' ve 'm' diye Evrenden İki Tane Doğal Sayı (nat) Verilirse, 
   (n + m) DAİMA VE KATİYYEN (m + n) ye Eşittir!!!! (Bunu Çözeceğiz)
*)
Theorem toplama_degismezdir : forall n m : nat, n + m = m + n.


(* 2. İSPATA (PROOF) KARANLIK ODAYA GİR VE ÇARPIŞ !! *)
Proof.
  (* (intros): Disaridan Verilen 'n' ve 'm' Degiskenlernii Benim Felsefe Masama Getir! (Sahneye sok) *)
  intros n m.
  
  (* (induction) Tümevarım Taktiğini Uygula!! 'n' üzerinden Gideceksin. 
     (Yani n yi önce 0 Alacaksın . Doğru çıkarsa. n yi n+1(Successor) alacaksın Yine Deneyeceğim! *)
  induction n as [| n' IHn'].
  
  - (* Birinci Durum! : (Asıl Durum- Base Case). Diyelim ki N = Sıfır (0).
       Sahne şuna Dönüşür: 0 + m = m + 0. Bunun Doğruluğunu İspatla! *)
    
    (* Coq'un icindeki Dahili Basitleştir() Simpl. Fonksiyonuna sokarız Ve 
       Kendi İçindeki (plus_n_O) yani (Bir Sayıya 0 Eklersen Kendisidir) kuralını uygulayarak Çözer! *)
    simpl. rewrite <- plus_n_O. reflexivity.
    
  - (* İkinci Durum (Adımlama - Inductive Step). Diyelim ki n değil de (n+1) gelseydi. 
       Mantık İçinde Şu Formülü Uygula diyoruz !*)
    
    simpl. rewrite -> IHn'. rewrite -> plus_n_Sm. reflexivity.


(* 3. ALKIŞ VE KIYAMET !!! İSPAT BAŞARI İLE ÇÖZÜMLENDİ (Quod Erat Demonstrandum - Q.E.D) *)
Qed.
```
Yukarıdaki bu `Qed` (Bitti!) Komutu Geldiğinde Coq Bilgisayarı "Evet! Gerçekten de A+B = B+A İspatlanmıştır" Der ve Altına EFSANEVİ Yeşil Çekiğini ATAR. Siz Coq Terminalinin İçine Bakarken "Tanrı İle Sohbet eden (Matematiğin Sırrını çizen)" Bir Filozof Gibisinizdir. Acı verir, ama Nükleer Reaktörleri Patlamaktan kurtaran O "Kusursuz Yazılım(Zero-Bug)" Büyüsüdür.

## Kimler Kullanır?
* Evrendeki sadece **En Elit, Akademik Felsefe Kökenli, Formal Doğrulama (Formal Verification) Mühendisleri**. Silikon vadisinde Uçak (Boeing), Uzay (Nasa Mekiği Füzeleri), Nükleer ve İnsan Hayatına kast Edecek Otomasyon Cihazlarının Yazılımlarındaki C/Assembly Olarak Çıkan Kodu Coq Motoruna Sokarak Gece Başında Bekleyen İnsanlardır. (Bu adamlara Saygıyla Eğilir, Selam verirsiniz). Dünyanın Programlama Ustalarıdırlar.

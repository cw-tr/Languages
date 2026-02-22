# Standard ML

## Özet
Standard ML (SML - Standard Meta Language); 1980'lerde, Lisp felsefesinin o "Parantez Karmaşasını" çöpe atan ama fonksiyonelliğin saf gücünü saklayarak Tip Güvenliğini (Hindley-Milner Types) matematiksel ispatlarıyla evrene getiren, OCaml ve Haskell gibi devasa endüstri fonksyonel dillerinin ana temeli/kök Atası olan harikulade Fonksiyonel Programlama dilidir.

## Nedir ve Ne İşe Yarar?
1970'lerde Edinburgh Üniversitesi'ndeki Robin Milner adındaki bir dahi, otomatik teorem ispatlayan ("Acaba bu bilgisayar fonksiyonunda matematiksel bir hata var mıdır" diye kendi kendine kanıtlama yapan) "LCF" adında bir yapay zeka programı yazıyordu. O programı daha hızlı ve mantıklı kontrol etmek için "Meta Language - LCF" adıyla bir dil icat etti. 

İşte bu Meta Language 1986'da herkesin kullanabileceği bir standarta oturtularak **Standard ML** adını aldı. İlk defa Lisp'in o listeli, zayıf tip hatalarından dünyayı kurtaran ve "**Statik Tip Çıkarım (Type Inference)**" denilen o kara büyüyü (Yazılımcı tip vermez, ama derleyici tüm her şeyi 1 milisaniyede tahmin edip çelikle mühürler) tarihe yazdıran dildir.

**Ne İşe Yarar?**
* **Kusursuz Derleyici Geliştirme:** Günümüzde bile dillerin (Örneğin Elm veya yeni çıkan sistem dillerinin) ilk derleyicileri SML ile taslaklanır (Bootstrapping). Çünkü SML'de yazdığınız bir kod derlemeyi (Compile) başarıyla geçiyorsa, **Hata Verme (Runtime Exception) ihtimali SIFIRA yakındır!** ("Well-typed programs cannot go wrong" - İyi tipleşmiş program yanlış gidemez sözü ML babalarına aittir).
* **Matematikçi/Felsefeci Akademisi:** Haskell gelmeden dünyada bu dilde saf veriyapılarını, teorem kanıtlarını kanıtlamak için (Örn: Isabelle Teorem İspatlayıcı SML dilinde yazılmıştır).

## Dilin Mantığı ve Kod Yapısı
OCaml (*Objective* Caml) Sınıflandırılmış ve nesneye yönelik (Object/Class) pazar halidir. Babadan Olan SML ise asla nesne-yönelim barındırmaz; tamamen saf Modüller (Signatures / Structures / Functors) üzerine odaklanan dörtdörtlük Cebirsel (Algebraic) Veri motorları kullanır. 

Sözdizimi çok durudur lakin Fonksiyonlar arası "Kısmi Uygulama (Currying)" denen ve iki değişkenden birini verdiğinde "yeni kopya bir fonksiyon üreten" harika fonksiyon şablonları döner.

**Örnek İşleyiş (Sembolik Olarak):**
Liste İşleme: `val sayilar = [1, 2, 3]`
Kondisyon/Atama: `val sonuç = if x > 5 then "Büyük" else "Küçük"` (Lisp'ten farklı olarak parantezler değil, İngilizce okunaklı metin bloğu ML tabanlıdır).

### Örnek Bir Standard ML Kodu: Fonksiyonel Eşleştirme (Pattern Match) ve Currying
Haskell, Scala, Rust dillerinin bile bugün aşırdığı ve "Böceksiz/Bug'suz Kod" efsanesini başlatan Pattern Eşitleme (Zarif Switch/Case devrimi) listesi:

```sml
(* SML Yorum Satirlari Parantez ve Yildiz kombinasyonlaridir *)

(* 1. CEBİRSEL (ALGEBRAIC) TİP YARATILMASI: *)
(* Biz C dilinde Enum degil, matematikteki (Agaçların) Dal tipini SML ile sekillendiriyoruz *)
datatype agac = 
    Bos_Yaprak 
  | Dugum of int * agac * agac;  (* Bir Sayi(int), Sol Agac, Sag Agac bekler *)


(* 2. MÜKEMMEL DESEN EŞLEŞTİRME (PATTERN MATCHING) - LISP VEYA FOR SOKKU YOK! *)
(* Ağaçtaki tüm sayilari Yalnizca Olasiliklari Mühürleyerek Topla! *)

fun agac_dallari_toplasi (Bos_Yaprak) = 0   (* Zemin / Sifir Cikis Olasiligi *)
  
  (* EGER Agaci bulursam İçindeki (Deger, SOL, SAG) desenini parcaliyorum ve Topluyorum! *)
  | agac_dallari_toplasi (Dugum(kendi_degeri, sol_Dal, sag_Dal)) =
      kendi_degeri + 
      agac_dallari_toplasi(sol_Dal) + 
      agac_dallari_toplasi(sag_Dal);


(* 3. ASIL ÇALIŞMA BÖLÜMÜ (VAL KOMUTU- Value Immutable) *)
(* Veri Hafizada degismez! *)

(* Dev gibi, saglı sollu Agaç Haritasını Ciziyoruz *)
val benim_orman = Dugum(5, 
                        Dugum(3, Bos_Yaprak, Bos_Yaprak), 
                        Dugum(8, Bos_Yaprak, Bos_Yaprak));


(* Toplama Fonkisyonunu Çagır Ekrana (Console'a) Karakter Olarak At *)
val asil_sonuc = agac_dallari_toplasi benim_orman;

(* Integer ToString yap -> (Int.toString) SML Format! *)
val _ = print ("Ormandaki Dallarin Toplam Degeri: " ^ Int.toString(asil_sonuc) ^ "\n");

(* Cikti: "Ormandaki Dallarin Toplam Degeri: 16" (5+3+8) *)  
```
Bu programı derlerken `SML/NJ` derleyicisi size derki: `val agac_dallari_toplasi : fonskyon (agac tipinden -> int tipine dondurucu)`. Sizin satır satır "Bu float mi? int mi?" yazmanıza gerek kalmaz, ML derleyicisi C# ve Java derleyicisinden bin kat zeki matematik algoritmasına (Hindley-Milner çıkarım bulutuna) sahiptir.

## Kimler Kullanır?
* "Ben C programı derleyicisine güvenmem, matematikle bu kodun Asla Çökmeyeceğini ispat etmeliyim" diyen Aşırı Elit Avrupalı/Amerikalı Akademik Teorem mühendisleri. 
* Ancak ticari kullanımdaki yerini Oğlu sayılan **OCaml**'e ve ruh kardeşi **Haskell**'e kaptırmıştır.
* Endüstride OCaml kullanılır (Jane Street vb. Tarafından), Akademi de Teorem ispatı için SML yaşatılır. Geriye kalan tüm dünyanın (C++, Rust, TypeScript, Swift dahil) *Pattern Matching (Eşleştirme), Option(Nullable) Monad'ı, ve Tip Çıkarımı (Auto / Var kalıpları)* borçlu olduğu gizli ilahiyat dilidir.

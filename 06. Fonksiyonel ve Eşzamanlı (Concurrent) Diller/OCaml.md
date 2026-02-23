# OCaml

## Özet
OCaml (Objective Caml); 1996 yılında Fransa'daki INRIA enstitüsü tarafından geliştirilen, Haskell kadar "Saf Fonksiyonel" olmasına rağmen OOP (Nesne Yönelimli) ve Emirsel (Imperative/Değişken değiştirebilen) özellikleri de bünyesinde barındırarak akademiyi gerçek sanayi/endüstri dünyasıyla birleştiren, muazzam güvenli **Statik Tipli Fonksiyonel Programlama** dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda Fonksiyonel diller (Lisp, Haskell, ML) genellikle sadece üniversite akademisyenlerinin "Yan Etkisiz (Side-Effect Free) Matematik İspatlamak" için kullandığı elit dillerdi. Şirketler ise "Bize hızlı çalışan, C gibi döngü atabilen ama Haskell gibi Tip-Güvenliği / Bug (Hata) geçirmeyen pratik bir dil lazım" diyordu.

INRIA, klasik 'ML (Meta Language)' dilinin üzerine Nesne Yönelimli (Sınıflar/Objeler) yetenekleri ekledi. OCaml doğdu. Siz OCaml ile tıpkı Haskell gibi mükemmel bir matematiksel fonksiyon yazabilirsiniz, ama canınız isterse "Ben bunu for döngüsüne sokup değişkenin değerini +1 yapacağım" da diyebilirsiniz (Impure). C'den biraz yavaş, Java'dan çok daha hızlıdır ve evrendeki en güçlü Tip Çıkarım (Type Inference) motorlarından birine sahiptir.

**Ne İşe Yarar?**
* **Finansal Algoritma Sistemleri (Quant):** Jane Street gibi Wall Street'te saniyede milyarlarca dolarlık borsa işlemi (HFT - High Frequency Trading) yapan dev şirketler, "Eğer kodda hata çıkarsa milyar dolar kaybederiz" korkusuyla Bütün sistemlerini %100 oranında (Böcek/Bug yasaklayan) OCaml ile yazmıştır!
* **Derleyici (Compiler) Üretimi:** Facebook'un yarattığı Hack(PHP C++) derleyicileri, Rust dilinin ilk orijinal Derleyicisi ve Ethereum Akıllı Kontrat derleyicileri ilk başta OCaml ile yazılmıştır. Yeni dillerin kurallarını kodlamak için en iyi 'Meta-Dildir'.

## Dilin Mantığı ve Kod Yapısı
OCaml, katı ve güçlü tiplidir (Strict/Static Typed). Ama Java gibi sizden `int x = 5` diye tip dilenmez. `let x = 5` yazarsınız, OCaml derleyicisi (Hindley-Milner Algoritması) "Ah, 5 bir tam sayıdır, o halde x hayatı boyunca 'int' olmak zorundadır" diye fısıldayarak Değişkeni ebediyyen kilitler. OCaml ile yazılıp "Compile(Derle)" tuşundan geçen bir kodun, çalışma esnasında (Runtime) patlama veya çökme ihtimali neredeyse SIFIRDIR.

Pattern Matching (Desen Eşleştirme) üzerine kuruludur. İf-Else zincirleriyle koca kodlar yazmak yerine, `match...with` kulpuna takılı pürüzsüz ihtimal matematiği döner.

**Örnek İşleyiş (Sembolik Olarak):**
Liste İşleme (Haskell Vari): `let listem = [1; 2; 3]`
Fonksiyon tanımlama: `let topla a b = a + b` (Hiçbir tip yazmadınız ama OCaml bunun 'int->int->int' olduğunu 1 milisaniyede tahmin edip mühürledi!)

### Örnek Bir OCaml Kodu: Desen Eşleştirme (Pattern Matching) Mucizesi
Programcı if/else spagettisi olmadan, Enum (Varyant) tipleriyle olasılıkları nasıl zarifçe tarayıp, fonksiyonel listelerde eşleştirir:

```ocaml
(* OCaml'da yorum satirlari (* ve *) isaretleri arasindadir *)

(* 1. MUCİZE TİPLER (Algebraic Data Types - Cebirsel Veri Tipleri) *)
(* Kendimize C Structlarindan milyon kat akilli bir Tip yaratıyoruz: *)

type sekil =
  | Daire of float              (* Yarıçap alanı Float bekler *)
  | Dikdortgen of float * float (* İki tane Float bekler (En, Boy) *)
  | Nokta                       (* Hicbir sey beklemez *)

(* 2. MUCİZE FONKSİYON VE DESEN EŞLEŞTİRME (Match ... with) *)
(* if-else kullanmak yasak! 'match' ile olasiliklari tariyoruz *)

let alan_hesapla (nesnem : sekil) : float =
  match nesnem with
  | Daire r -> 3.14159 *. (r *. r)   (* OCaml'da Float Çarpımı için normal '*' degil, '*.' kullanilir! *)
  
  | Dikdortgen (en, boy) -> en *. boy
  
  | Nokta -> 0.0                     (* Hata verdirmez, tum olasiliklari kapsattirir! *)


(* 3. LİSTELERİ İŞLEME VE DÖNGÜSÜZ (MAP) OPERASYONU *)
let main () =
  
  (* Sekillerden Olusan Bir Liste Yarat (Noktali virgul [ ; ; ] ile ayrilir) *)
  let benim_sekiller = [ Daire 5.0; Dikdortgen (4.0, 5.0); Nokta ] in
  
  (* Imperative (Emirsel C dili) bir FOR dongusu degil, Fonksiyonel 'List.map' (Yansitma) kullaniyoruz *)
  let alanlar_listesi = List.map alan_hesapla benim_sekiller in
  
  (* Ekrana Bas! (Printf kütüphanesi Imperative / Side-effect'tir) *)
  Printf.printf "Dairenin Cikan Alani: %f\n" (List.hd alanlar_listesi);
  
  print_endline "Tum Olasiliklar Basariyla (Ve hatasiz) Hesaplandi."

(* Fonksiyonu Cagirarak Programi Yürüt *)
let () = main ()
```
OCaml derleyicisi o kadar zekidir ki; yukarıdaki `match` bloğuna biz "Nokta" ihtimalini eklemeseydik, kod çalışırken değil **"Siz Daha Dosyayı Derlemeye (Compile) Çalıştığınız Saniye"** "HEY! Nokta ihtimalini girmedin, ileride programın bu satırda çökebilir, derlemeyi Reddediyorum!" diyerek kodu suratınıza çarpar. Milyar dolarlık borsaları böyle korurlar!

## Kimler Kullanır?
* Wall Street Finans (High Frequency Trading) Hackerları (Jane Street ve Bloomberg Quant Mühendisleri).
* "Coq" gibi dünyanın en gelişmiş Matematik Teorem İspatlayıcılarının arka ocağı geliştiricileri.
* Meta/Facebook içinde devasa kod analiz kütüphanelerini yazan Dil-Tasarım mimarları. (OCaml'ın JS'ye benzer ve okunabilir syntaxlı versiyonu olan *ReasonML* de Facebook tarafından Web front-end için icat edilmiştir).
* Son derece Akademik-Sanayi Kırması, pürüzsüz zeka isteyen Avrupalı dâhi beyinler (INRIA-Fransa kökenlidir).
